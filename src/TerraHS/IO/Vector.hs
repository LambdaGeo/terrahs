-- | Joins the geometry of a @.shp@ file with the attributes of the
-- matching @.dbf@ — the pair that, in the original TerraHS, was
-- loaded in one call with something like
-- @loadVectorFile \"MG_MUN96.shp\"@.
module TerraHS.IO.Vector
  ( VectorFeature (..)
  , readVectorFile
  ) where

import System.FilePath (replaceExtension)

import TerraHS.Geometry.Any (AnyGeometry)
import TerraHS.IO.Dbf (DbfTable (..), DbfValue, readDbf)
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
