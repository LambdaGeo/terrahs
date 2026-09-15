-- | Reading the main file of a Shapefile (@.shp@) — the format the
-- original TerraHS consumed via TerraLib
-- (@loadVectorFile \"MG_MUN96.shp\"@).
--
-- Implemented with @Data.Binary.Get@ from the @binary@ library, with
-- no dependency on GDAL\/OGR. It is a hand-written binary parser, a
-- good exercise in how the format mixes big-endian (headers) with
-- little-endian (almost everything else) — a quirky design choice
-- from ESRI's original 1990s specification.
--
-- Covers shape types 0 (Null), 1 (Point), 3 (PolyLine), and 5
-- (Polygon) — the non-Z\/M equivalents of the types
-- 'TerraHS.Geometry' already models. It does not distinguish between
-- an outer ring and a hole in polygons (each Shapefile ring becomes
-- an independent 'Polygon') — left as a future step, alongside
-- reading the @.dbf@ for attributes (the equivalent of the old
-- @TeTable@).
--
-- Does not read the @.shx@ (index) or @.dbf@ (attributes) files —
-- only the @.shp@, which is already enough to recover the geometry.
module TerraHS.IO.Shapefile
  ( readShapefile
  , parseShapefile
  , readShapefileRecords
  , parseShapefileRecords
  , ShapeType (..)
  ) where

import Control.Monad (replicateM)
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32)

import TerraHS.Geometry.Coord (Coord (..))
import TerraHS.Geometry.Point (Point (..))
import TerraHS.Geometry.Line (mkLine)
import TerraHS.Geometry.Polygon (mkPolygon)
import TerraHS.Geometry.Any (AnyGeometry (..))

-- | The shape types this parser supports (a subset of those defined
-- by the ESRI specification).
data ShapeType = STNull | STPoint | STPolyLine | STPolygon
  deriving (Eq, Show)

shapeTypeFromCode :: Int32 -> Get ShapeType
shapeTypeFromCode 0 = pure STNull
shapeTypeFromCode 1 = pure STPoint
shapeTypeFromCode 3 = pure STPolyLine
shapeTypeFromCode 5 = pure STPolygon
shapeTypeFromCode n = fail ("unsupported shape type (Z/M or MultiPoint?): " ++ show n)

-- | The 100-byte header at the start of the file. Fields not used
-- here (version, global bounding box) are only documented — anyone
-- who needs them can expose this function.
skipHeader :: Get ()
skipHeader = do
  fileCode <- getInt32be
  if fileCode /= 9994
    then fail ("file does not look like a valid Shapefile — expected file code 9994, got " ++ show fileCode)
    else do
      skip (5 * 4)      -- 5 unused integers
      _fileLength <- getInt32be -- in 16-bit words, including the header
      _version    <- getInt32le
      _shapeType  <- getInt32le
      skip (8 * 8)      -- global bounding box: Xmin,Ymin,Xmax,Ymax,Zmin,Zmax,Mmin,Mmax

-- | A record is a pair (record number, content already turned into
-- geometries — a record can become more than one 'AnyGeometry' if the
-- original shape has multiple parts, e.g. a polygon with holes or a
-- multi-part polyline).
getRecord :: Get [AnyGeometry]
getRecord = do
  _recordNumber <- getInt32be
  _contentLen   <- getInt32be -- in 16-bit words
  shapeTypeCode <- getInt32le
  ty            <- shapeTypeFromCode shapeTypeCode
  case ty of
    STNull     -> pure []
    STPoint    -> do
      x <- getDoublele
      y <- getDoublele
      pure [AGPoint (Point (Coord x y))]
    STPolyLine -> getMultiPart toLine
    STPolygon  -> getMultiPart toPolygon
  where
    toLine cs    = [AGLine l | Just l <- [mkLine cs]]
    toPolygon cs = [AGPolygon p | Just p <- [mkPolygon cs]]

-- | Reads the body common to PolyLine\/Polygon: the record's bounding
-- box, the starting index of each part, and the list of points — then
-- splits the points into one coordinate list per part.
getMultiPart :: ([Coord] -> [AnyGeometry]) -> Get [AnyGeometry]
getMultiPart toGeometries = do
  skip (4 * 8) -- record bounding box: Xmin,Ymin,Xmax,Ymax
  numParts  <- getInt32le
  numPoints <- getInt32le
  partStarts <- replicateM (fromIntegral numParts) getInt32le
  points     <- replicateM (fromIntegral numPoints) (Coord <$> getDoublele <*> getDoublele)
  let starts       = map fromIntegral partStarts ++ [fromIntegral numPoints]
      partRanges   = zip starts (drop 1 starts)
      partsOfCoord = [ take (end - start) (drop start points) | (start, end) <- partRanges ]
  pure (concatMap toGeometries partsOfCoord)

getShapefileRecords :: Get [[AnyGeometry]]
getShapefileRecords = do
  skipHeader
  getRecords
  where
    getRecords = do
      done <- isEmpty
      if done
        then pure []
        else (:) <$> getRecord <*> getRecords

-- | Parses the bytes of a @.shp@ file, preserving each record's
-- boundary — a list of geometries per record (almost always just
-- one, more than one when the original shape is multi-part). Use this
-- version when you need to line the geometry up with attributes from
-- a corresponding @.dbf@, since the order and count of records must
-- match the table's rows.
parseShapefileRecords :: BL.ByteString -> Either String [[AnyGeometry]]
parseShapefileRecords bytes =
  case runGetOrFail getShapefileRecords bytes of
    Left (_, _, err)      -> Left err
    Right (_, _, records) -> Right records

-- | Reads and parses a @.shp@ file from disk, preserving each
-- record's boundary. See 'parseShapefileRecords'.
readShapefileRecords :: FilePath -> IO (Either String [[AnyGeometry]])
readShapefileRecords path = parseShapefileRecords <$> BL.readFile path

-- | Parses the bytes of a @.shp@ file already loaded in memory,
-- flattening all records into a single list of geometries. Use this
-- version when you don't need to match against a @.dbf@'s attributes.
parseShapefile :: BL.ByteString -> Either String [AnyGeometry]
parseShapefile = fmap concat . parseShapefileRecords

-- | Reads and parses a @.shp@ file from disk, flattening all records
-- into a single list. See 'parseShapefile'.
readShapefile :: FilePath -> IO (Either String [AnyGeometry])
readShapefile path = parseShapefile <$> BL.readFile path
