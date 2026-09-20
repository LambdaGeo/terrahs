-- | road-city-join-demo: loads two real Shapefiles — a layer of
-- municipality polygons and a layer of road polylines — via
-- 'TerraHS.IO.Vector.readVectorFile', and answers, for each road,
-- "which municipalities does it cross?".
--
-- This is a spatial join by "crosses" rather than "contains" (the
-- predicate 'geojoin-demo' uses via 'pointInPolygon'). TerraHS's
-- 'TerraHS.Geometry.Topology' module doesn't have an exact
-- line-crosses-polygon test yet (only a bounding-box 'intersects', a
-- point-on-line test, and a point-in-polygon test — the module's own
-- haddock flags the exact segment-to-segment case as still pending).
-- So this demo builds 'crossesPolygon' locally, on top of the
-- primitives that already exist: a standard 2D segment-intersection
-- test, checked against every edge of the polygon, plus
-- 'pointInPolygon' as a shortcut for the case where a road vertex
-- itself lies inside.
--
-- Shapefile-to-'Coverage' plumbing goes through 'asPolygonPairs' \/
-- 'asLinePairs' and 'attrAs' (from "TerraHS.IO.Vector") and
-- 'fromPairs' (from "TerraHS.Algebra.Coverage") — the three
-- additions that replace what used to be a hand-rolled
-- pattern-match-and-lookup dance in an earlier version of this file.
module Main (main) where

import Data.List (intercalate)
import System.IO (hSetEncoding, stdout, utf8)

import TerraHS

-- * A local line-crosses-polygon test
--
-- Not (yet) part of TerraHS.Geometry.Topology — see the module
-- haddock above.

-- | Twice the signed area of the triangle (p, q, r) — its sign gives
-- the turn direction (>0 counter-clockwise, <0 clockwise, 0
-- collinear). The standard building block for segment intersection.
orientation :: Coord -> Coord -> Coord -> Double
orientation (Coord px py) (Coord qx qy) (Coord rx ry) =
  (qx - px) * (ry - py) - (qy - py) * (rx - px)

-- | Do two segments cross? A simplified generic-position test (via
-- 'orientation' sign changes) — it doesn't special-case exact
-- collinear overlap, which is enough for this demo's data but would
-- need extending for a general-purpose library function.
segmentsIntersect :: (Coord, Coord) -> (Coord, Coord) -> Bool
segmentsIntersect (p1, p2) (p3, p4) =
  let o1 = orientation p1 p2 p3
      o2 = orientation p1 p2 p4
      o3 = orientation p3 p4 p1
      o4 = orientation p3 p4 p2
  in (signum o1 /= signum o2) && (signum o3 /= signum o4)

-- | A road crosses a municipality if any of its segments crosses one
-- of the polygon's edges, or if any of its vertices lies inside the
-- polygon (covers the case of a road segment starting or ending
-- inside, rather than passing all the way through).
crossesPolygon :: Line -> Polygon -> Bool
crossesPolygon line poly =
  any (\v -> pointInPolygon (Point v) poly) (lineCoords line)
    || any (\seg -> any (segmentsIntersect seg) polyEdges) (lineSegments line)
  where
    ring      = polygonRing poly
    polyEdges = zip ring (drop 1 ring)

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

main :: IO ()
main = do
  hSetEncoding stdout utf8

  putStrLn "== road-city-join-demo: which municipalities does each road cross? =="
  putStrLn ""

  citiesResult <- loadCities "data/cities.shp"
  roadsResult  <- loadRoads  "data/roads.shp"

  case (citiesResult, roadsResult) of
    (Left err, _) -> error ("failed to read data/cities.shp: " ++ err)
    (_, Left err) -> error ("failed to read data/roads.shp: " ++ err)
    (Right citiesCov, Right roads) -> do
      putStrLn (show (numElems citiesCov) ++ " municipalities loaded from data/cities.shp:")
      mapM_ (\poly -> putStrLn ("  " ++ covFun citiesCov poly ++ ", area = " ++ show (area poly)))
            (domain citiesCov)

      putStrLn ""
      putStrLn (show (length roads) ++ " roads loaded from data/roads.shp:")
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
