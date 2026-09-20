-- | road-city-join-demo: loads two real Shapefiles — a layer of
-- municipality polygons and a layer of road polylines — via
-- 'TerraHS.IO.Vector.readVectorFile', and answers, for each road,
-- "which municipalities does it cross?".
--
-- This is a spatial join by "crosses" (via
-- 'TerraHS.Geometry.Topology.crossesPolygon') rather than "contains"
-- (the predicate 'geojoin-demo' uses via 'pointInPolygon') — same
-- 'select'/'compose' pattern, different predicate.
--
-- Shapefile-to-'Coverage' plumbing goes through 'asPolygonPairs' \/
-- 'asLinePairs' and 'attrAs' (from "TerraHS.IO.Vector") and
-- 'fromPairs' (from "TerraHS.Algebra.Coverage").
module Main (main) where

import Data.List (intercalate)
import System.IO (hSetEncoding, stdout, utf8)

import TerraHS

-- * Loading the two layers straight into Coverages

-- | A shapefile of municipality polygons, keyed by their @NAME@
-- attribute — one line, no manual 'AnyGeometry' pattern matching or
-- 'DbfValue' unwrapping.
loadCities :: FilePath -> IO (Either String (Coverage Polygon String))
loadCities path = fmap toCoverage <$> readVectorFile path
  where
    toCoverage feats =
      fromPairs [ (poly, name)
                | (poly, attrs) <- asPolygonPairs feats
                , Just name <- [attrAs "NAME" attrs]
                ]

-- | Same idea for the road layer — a @(name, Line)@ per road is
-- enough here, since roads are what we test *against* (the `ref`
-- side of 'select'), not what we build a 'Coverage' domain out of.
loadRoads :: FilePath -> IO (Either String [(String, Line)])
loadRoads path = fmap toPairs <$> readVectorFile path
  where
    toPairs feats =
      [ (name, l) | (l, attrs) <- asLinePairs feats, Just name <- [attrAs "NAME" attrs] ]

dataDir :: FilePath
dataDir = "examples/data"

main :: IO ()
main = do
  hSetEncoding stdout utf8

  putStrLn "== road-city-join-demo: which municipalities does each road cross? =="
  putStrLn ""

  citiesResult <- loadCities (dataDir ++ "/cities.shp")
  roadsResult  <- loadRoads  (dataDir ++ "/roads.shp")

  case (citiesResult, roadsResult) of
    (Left err, _) -> error ("failed to read cities.shp: " ++ err)
    (_, Left err) -> error ("failed to read roads.shp: " ++ err)
    (Right citiesCov, Right roads) -> do
      putStrLn (show (numElems citiesCov) ++ " municipalities loaded from cities.shp:")
      mapM_ (\poly -> putStrLn ("  " ++ covFun citiesCov poly ++ ", area = " ++ show (area poly)))
            (domain citiesCov)

      putStrLn ""
      putStrLn (show (length roads) ++ " roads loaded from roads.shp:")
      mapM_ (\(name, l) -> putStrLn ("  " ++ name ++ ", length = " ++ show (lineLength l))) roads

      -- select's predicate has type (a -> ref -> Bool); here that's
      -- (Polygon -> Line -> Bool), so `crossesPolygon` just needs its
      -- two arguments flipped to fit.
      let crossesRoad poly roadLine = crossesPolygon roadLine poly

      putStrLn ""
      putStrLn "== Spatial join: municipalities crossed by each road =="
      mapM_
        (\(roadName, roadLine) -> do
            let crossed = values (select citiesCov crossesRoad roadLine)
            putStrLn ("  " ++ roadName ++ " crosses: "
                       ++ if null crossed then "(none)" else intercalate ", " crossed))
        roads

      putStrLn ""
      putStrLn "== Sanity checks (hand-computed expectations) =="
      let crossedBy name =
            values (select citiesCov crossesRoad (maybe (error "unknown road") id (lookup name roads)))
          checks =
            [ ("BR-010 crosses Cidade A and Cidade B, not Cidade C",
                crossedBy "BR-010" == ["Cidade A", "Cidade B"])
            , ("Vicinal crosses only Cidade C (no road vertex ever lands inside it)",
                crossedBy "Vicinal" == ["Cidade C"])
            ]
      mapM_ (\(name, ok) -> putStrLn ("  [" ++ (if ok then "OK" else "FAIL") ++ "] " ++ name)) checks
      if all snd checks
        then putStrLn "\nAll checks passed."
        else error "road-city-join-demo: a sanity check failed"
