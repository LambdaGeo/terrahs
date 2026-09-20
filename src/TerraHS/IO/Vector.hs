{-# LANGUAGE FlexibleInstances #-}
-- | Joins the geometry of a @.shp@ file with the attributes of the
-- matching @.dbf@ — the pair that, in the original TerraHS, was
-- loaded in one call with something like
-- @loadVectorFile \"MG_MUN96.shp\"@.
--
-- 'FlexibleInstances' is needed for the 'FromDbfValue' @[Char]@
-- instance below — @String@'s underlying type, @[Char]@, isn't of
-- the @T a1 .. an@ shape Haskell2010 requires for an instance head.
module TerraHS.IO.Vector
  ( VectorFeature (..)
  , readVectorFile
    -- * Pulling typed (geometry, attributes) pairs out of features
    --
    -- The usual next step after 'readVectorFile': keep only the
    -- features of one geometry type, paired with their attribute
    -- row, ready to hand to 'TerraHS.Algebra.Coverage.fromPairs'. A
    -- feature with more than one geometry (a multi-part shape)
    -- contributes one pair per part, all sharing that feature's
    -- attributes; a feature of the wrong geometry type is dropped.
  , asPointPairs
  , asLinePairs
  , asPolygonPairs
    -- * Reading a single attribute
  , FromDbfValue (..)
  , attrAs
  ) where

import System.FilePath (replaceExtension)

import TerraHS.Geometry.Any (AnyGeometry (..))
import TerraHS.Geometry.Point (Point)
import TerraHS.Geometry.Line (Line)
import TerraHS.Geometry.Polygon (Polygon)
import TerraHS.IO.Dbf (DbfTable (..), DbfValue (..), readDbf)
import TerraHS.IO.Shapefile (readShapefileRecords)

-- | A feature: the geometry of a @.shp@ record (usually just one,
-- more than one if the original shape is multi-part) together with
-- the attributes of the matching @.dbf@ row.
data VectorFeature = VectorFeature
  { featureGeometries :: [AnyGeometry]
  , featureAttributes :: [(String, DbfValue)]
  } deriving (Eq, Show)

-- | Reads a @.shp@ and the @.dbf@ with the same base name (in the
-- same folder) and joins them by record order — the same convention
-- the format uses: record N of the @.shp@ corresponds to row N of the
-- @.dbf@.
--
-- Pass the @.shp@ path; the @.dbf@ path is derived by swapping the
-- extension.
readVectorFile :: FilePath -> IO (Either String [VectorFeature])
readVectorFile shpPath = do
  let dbfPath = replaceExtension shpPath ".dbf"
  geomsResult <- readShapefileRecords shpPath
  dbfResult   <- readDbf dbfPath
  pure $ do
    geomsPerRecord <- geomsResult
    table          <- dbfResult
    let attrsPerRecord = dbfRecords table
        numGeomRecords  = length geomsPerRecord
        numAttrRecords  = length attrsPerRecord
    if numGeomRecords /= numAttrRecords
      then Left
        ( "number of .shp records (" ++ show numGeomRecords
          ++ ") does not match the number of .dbf rows (" ++ show numAttrRecords
          ++ ") — incompatible or corrupted files" )
      else Right (zipWith VectorFeature geomsPerRecord attrsPerRecord)

-- | Keeps only the point parts of each feature, paired with that
-- feature's attributes.
asPointPairs :: [VectorFeature] -> [(Point, [(String, DbfValue)])]
asPointPairs feats =
  [ (p, featureAttributes f) | f <- feats, AGPoint p <- featureGeometries f ]

-- | Keeps only the line parts of each feature, paired with that
-- feature's attributes.
asLinePairs :: [VectorFeature] -> [(Line, [(String, DbfValue)])]
asLinePairs feats =
  [ (l, featureAttributes f) | f <- feats, AGLine l <- featureGeometries f ]

-- | Keeps only the polygon parts of each feature, paired with that
-- feature's attributes.
asPolygonPairs :: [VectorFeature] -> [(Polygon, [(String, DbfValue)])]
asPolygonPairs feats =
  [ (poly, featureAttributes f) | f <- feats, AGPolygon poly <- featureGeometries f ]

-- | Types a @.dbf@ attribute can be read as, so callers don't pattern
-- match on 'DbfValue''s constructors themselves — in the spirit of
-- aeson's @FromJSON@, which "TerraHS.IO.GeoJSON" already leans on.
class FromDbfValue a where
  fromDbfValue :: DbfValue -> Maybe a

-- Written as @[Char]@, not the @String@ type synonym, so this
-- compiles under plain Haskell2010 without needing
-- @TypeSynonymInstances@.
instance FromDbfValue [Char] where
  fromDbfValue (DbfText s) = Just s
  fromDbfValue _           = Nothing

instance FromDbfValue Double where
  fromDbfValue (DbfNumber n) = Just n
  fromDbfValue _             = Nothing

instance FromDbfValue Bool where
  fromDbfValue (DbfBool (Just b)) = Just b
  fromDbfValue _                  = Nothing

-- | Looks up one attribute by field name from an attribute row (as
-- returned by 'asPointPairs' and friends, or 'featureAttributes'),
-- reading it as whichever type is expected at the call site.
-- 'Nothing' if the field is missing, empty ('DbfNull'), or doesn't
-- match the requested type.
--
-- >>> attrAs "NAME" [("NAME", DbfText "Cidade A")] :: Maybe String
-- Just "Cidade A"
attrAs :: FromDbfValue a => String -> [(String, DbfValue)] -> Maybe a
attrAs k attrs = lookup k attrs >>= fromDbfValue
