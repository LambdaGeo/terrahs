-- | ibge-road-join-demo: the same "which municipalities does this
-- road cross?" spatial join as road-city-join-demo, but against
-- REAL municipal boundaries this time — IBGE's 2025 Malha Municipal
-- for Maranhão (217 municipalities, SIRGAS2000 lat/long,
-- data\/ibge\/MA_Municipios_2025.shp\/.dbf).
--
-- There's no real road layer to pair it with (none was available),
-- so the "road" below is INVENTED — a hand-drawn line, not GPS or
-- OpenStreetMap data — laid across the Ilha do Maranhão area (São
-- Luís and its neighboring municipalities) purely so the join has
-- something to select against. Every result printed is a real
-- consequence of that fictional line against real polygon boundaries
-- — not a canned answer — so whatever municipalities come out are
-- whatever the geometry actually says, not a target we aimed for.
--
-- This demo also shows carrying more than one attribute per domain
-- element in a 'Coverage': 'CityInfo' bundles the municipality's name
-- (@NM_MUN@) and its official area in km² (@AREA_KM2@, straight from
-- the IBGE table — not the shoelace-formula 'area' TerraHS computes
-- from the geometry itself), built with the 'Maybe' 'Applicative' so
-- a feature missing either field is dropped rather than crashing.
module Main (main) where

import Data.List (intercalate)
import System.IO (hSetEncoding, stdout, utf8)

import TerraHS

-- * A local line-crosses-polygon test (see road-city-join-demo for
-- the same code with more commentary on why it's here rather than in
-- TerraHS.Geometry.Topology).

orientation :: Coord -> Coord -> Coord -> Double
orientation (Coord px py) (Coord qx qy) (Coord rx ry) =
  (qx - px) * (ry - py) - (qy - py) * (rx - px)

segmentsIntersect :: (Coord, Coord) -> (Coord, Coord) -> Bool
segmentsIntersect (p1, p2) (p3, p4) =
  let o1 = orientation p1 p2 p3
      o2 = orientation p1 p2 p4
      o3 = orientation p3 p4 p1
      o4 = orientation p3 p4 p2
  in (signum o1 /= signum o2) && (signum o3 /= signum o4)

crossesPolygon :: Line -> Polygon -> Bool
crossesPolygon line poly =
  any (\v -> pointInPolygon (Point v) poly) (lineCoords line)
    || any (\seg -> any (segmentsIntersect seg) polyEdges) (lineSegments line)
  where
    ring      = polygonRing poly
    polyEdges = zip ring (drop 1 ring)

-- * Loading the real municipality layer, with two attributes per city

-- | A municipality's name and official area — both straight from the
-- IBGE @.dbf@, not computed.
data CityInfo = CityInfo
  { cityName    :: String
  , cityAreaKm2 :: Double
  } deriving (Eq, Show)

loadCities :: FilePath -> IO (Either String (Coverage Polygon CityInfo))
loadCities path = fmap toCoverage <$> readVectorFile path
  where
    toCoverage feats =
      fromPairs [ (poly, info)
                | (poly, attrs) <- asPolygonPairs feats
                , Just info <- [attrRecord attrs]
                ]
    attrRecord attrs = CityInfo <$> attrAs "NM_MUN" attrs <*> attrAs "AREA_KM2" attrs

-- * The fictional road
--
-- NOT real data: a hand-drawn line crossing the Ilha do Maranhão
-- area (longitude around -44.45 to -44.00, latitude around -2.50 to
-- -2.46 — picked to run through where São Luís and its neighboring
-- municipalities sit, nothing more precise than that).
fictionalRoad :: Line
fictionalRoad =
  maybe (error "invariant violated") id
    (mkLine
      [ Coord (-44.45) (-2.50)
      , Coord (-44.30) (-2.47)
      , Coord (-44.15) (-2.49)
      , Coord (-44.00) (-2.46)
      ])

main :: IO ()
main = do
  hSetEncoding stdout utf8

  putStrLn "== ibge-road-join-demo: real municipalities, a fictional road =="
  putStrLn ""
  putStrLn "Municipalities: IBGE Malha Municipal 2025, Maranhão (real data)."
  putStrLn "Road: hand-drawn, NOT real GPS/OSM data -- see the source for its exact coordinates."
  putStrLn ""

  citiesResult <- loadCities "data/ibge/MA_Municipios_2025.shp"

  case citiesResult of
    Left err -> error ("failed to read the IBGE shapefile: " ++ err)
    Right citiesCov -> do
      putStrLn (show (numElems citiesCov) ++ " municipality polygon parts loaded (217 municipalities, "
                 ++ "some split into several parts -- islands, mostly).")

      let crossesRoad poly _ = crossesPolygon fictionalRoad poly
          crossed = select citiesCov crossesRoad fictionalRoad
          -- A municipality can appear more than once if more than one
          -- of its parts is crossed (shouldn't happen for a single
          -- straight-ish line, but de-duplicate by name to be safe).
          crossedNames = dedupe [ cityName info | info <- values crossed ]
          dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

      putStrLn ""
      putStrLn "== Spatial join: which municipalities does the fictional road cross? =="
      if null crossedNames
        then putStrLn "  (none -- the line didn't land inside any real municipal boundary)"
        else mapM_
               (\info -> putStrLn ("  " ++ cityName info ++ ", area = " ++ show (cityAreaKm2 info) ++ " km2"))
               [ info | info <- values crossed, cityName info `elem` crossedNames ]

      putStrLn ""
      putStrLn ("Crossed " ++ show (length crossedNames) ++ " distinct municipalit"
                 ++ (if length crossedNames == 1 then "y" else "ies")
                 ++ ": " ++ intercalate ", " crossedNames)
      putStrLn "(this is the real result of testing the fictional line above against the"
      putStrLn " real IBGE polygons -- not picked in advance; check it against what you"
      putStrLn " know of the area.)"
