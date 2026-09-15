{-# LANGUAGE OverloadedStrings #-}

-- | Reading and writing GeoJSON (RFC 7946), the part that matters
-- here: geometry objects (@Point@, @LineString@, @Polygon@).
--
-- Unlike 'TerraHS.IO.WKT', this module uses the @aeson@ library —
-- GeoJSON is, in practice, today's most interoperable format (QGIS,
-- Leaflet, PostGIS, and so on read and write it without conversion),
-- so it makes no sense to reinvent a JSON parser just for the sake of
-- dependency purity.
--
-- Current limitation: it only handles a single geometry object at a
-- time, not @Feature@\/@FeatureCollection@ (which carry attributes
-- alongside geometry) — that will make more sense once TerraHS has an
-- attribute\/table type, like the old @TeTable@.
module TerraHS.IO.GeoJSON
  ( decodeGeoJSON
  , decodeGeoJSONFile
  , encodeGeoJSON
  , encodeGeoJSONFile
  ) where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , eitherDecode
  , encode
  , object
  , withObject
  , (.:)
  , (.=)
  )
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)

import TerraHS.Geometry.Coord (Coord (..))
import TerraHS.Geometry.Point (Point (..))
import TerraHS.Geometry.Line (Line (..), mkLine)
import TerraHS.Geometry.Polygon (Polygon (..), mkPolygon)
import TerraHS.Geometry.Any (AnyGeometry (..))

pairToCoord :: [Double] -> Parser Coord
pairToCoord [x, y] = pure (Coord x y)
pairToCoord xs     = fail ("invalid GeoJSON coordinate, expected [x, y]: " ++ show xs)

coordToPair :: Coord -> [Double]
coordToPair (Coord x y) = [x, y]

instance FromJSON AnyGeometry where
  parseJSON = withObject "Geometry" $ \o -> do
    ty <- o .: "type" :: Parser Text
    case ty of
      "Point" -> do
        raw <- o .: "coordinates" :: Parser [Double]
        c   <- pairToCoord raw
        pure (AGPoint (Point c))

      "LineString" -> do
        raw <- o .: "coordinates" :: Parser [[Double]]
        cs  <- mapM pairToCoord raw
        case mkLine cs of
          Just l  -> pure (AGLine l)
          Nothing -> fail "LineString needs at least 2 points"

      "Polygon" -> do
        rings <- o .: "coordinates" :: Parser [[[Double]]]
        case rings of
          (exterior : _interiorRingsIgnoredForNow) -> do
            cs <- mapM pairToCoord exterior
            case mkPolygon cs of
              Just p  -> pure (AGPolygon p)
              Nothing -> fail "Polygon needs at least 3 distinct vertices"
          [] -> fail "Polygon with no rings in \"coordinates\""

      other -> fail ("unsupported GeoJSON geometry type: " ++ show other)

instance ToJSON AnyGeometry where
  toJSON (AGPoint (Point c)) =
    object ["type" .= ("Point" :: Text), "coordinates" .= coordToPair c]
  toJSON (AGLine (Line cs)) =
    object ["type" .= ("LineString" :: Text), "coordinates" .= map coordToPair cs]
  toJSON (AGPolygon (Polygon ring)) =
    object ["type" .= ("Polygon" :: Text), "coordinates" .= [map coordToPair ring]]

-- | Decodes a GeoJSON geometry object from bytes.
decodeGeoJSON :: BL.ByteString -> Either String AnyGeometry
decodeGeoJSON = eitherDecode

-- | Reads and decodes a @.geojson@ file.
decodeGeoJSONFile :: FilePath -> IO (Either String AnyGeometry)
decodeGeoJSONFile path = decodeGeoJSON <$> BL.readFile path

-- | Serializes a geometry to GeoJSON bytes.
encodeGeoJSON :: AnyGeometry -> BL.ByteString
encodeGeoJSON = encode

-- | Serializes and writes a geometry to a @.geojson@ file.
encodeGeoJSONFile :: FilePath -> AnyGeometry -> IO ()
encodeGeoJSONFile path = BL.writeFile path . encodeGeoJSON
