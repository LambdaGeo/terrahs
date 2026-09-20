-- | geojoin-demo: a focused demonstration of the spatial join /
-- zonal-statistics operation, built entirely from the thesis's
-- 'Coverage' algebra (see "TerraHS.Algebra.Coverage").
--
-- A "spatial join" in GIS terms — attaching, to each polygon of a
-- layer, some aggregate of the points that fall inside it — is
-- exactly what the thesis's generalized ZONAL operator does: a
-- 'select' (spatial predicate against a reference geometry) followed
-- by a 'compose' (aggregation), or both at once via 'spatial'. This
-- demo makes that single point concrete with a small, hand-checkable
-- dataset, and contrasts it with the non-spatial join done by
-- 'multiple' (matching by domain element, not by geometry).
module Main (main) where

import TerraHS
import System.IO (hSetEncoding, stdout, utf8)

-- * Synthetic data
--
-- Fixed, hand-chosen coordinates (not randomly generated) so every
-- number printed below can be verified by inspection — see the
-- worked expectations in the comments and the checks at the end of
-- 'main'.

-- | Three non-overlapping municipalities.
zoneA, zoneB, zoneC :: Polygon
zoneA = maybe (error "invariant violated") id (mkPolygon [Coord 0 0, Coord 4 0, Coord 4 4, Coord 0 4])
zoneB = maybe (error "invariant violated") id (mkPolygon [Coord 5 0, Coord 9 0, Coord 9 4, Coord 5 4])
zoneC = maybe (error "invariant violated") id (mkPolygon [Coord 0 5, Coord 4 5, Coord 4 9, Coord 0 9])

zoneNames :: [String]
zoneNames = ["A", "B", "C"]

-- | The reference coverage for the join. Its own values are
-- irrelevant to 'spatial' (only its *domain* — the polygons — is
-- used to select and to shape the result); 'zoneNames', indexed by
-- domain position, is what supplies the human-readable labels below.
zones :: Coverage Polygon Double
zones = newCov [zoneA, zoneB, zoneC] (const 0)

-- | Eight rain-gauge stations: an id, a coordinate, and a rainfall
-- reading (mm). The last one sits outside all three zones on
-- purpose, to show that a spatial join simply drops points that
-- don't match any zone rather than erroring.
stations :: [(String, Coord, Double)]
stations =
  [ ("s1", Coord 1 1,   120)  -- zone A
  , ("s2", Coord 3 3,   150)  -- zone A
  , ("s3", Coord 2 1,    90)  -- zone A
  , ("s4", Coord 6 1,   200)  -- zone B
  , ("s5", Coord 7 3,   180)  -- zone B
  , ("s6", Coord 1 6,    60)  -- zone C
  , ("s7", Coord 3 7,    80)  -- zone C
  , ("s8", Coord 10 10, 300)  -- outside every zone
  ]

-- | The rainfall readings as a 'Coverage' over 'Point's — the input
-- side of the join.
rainfall :: Coverage Point Double
rainfall = newCov [ Point c | (_, c, _) <- stations ]
                   (\p -> head [ v | (_, c, v) <- stations, Point c == p ])

-- | Station metadata (elevation, in meters), keyed by the same
-- points, kept separate on purpose — joined back to 'rainfall' below
-- via 'multiple', a *non-spatial* local join, to contrast with the
-- spatial join that follows.
elevation :: Coverage Point Double
elevation = newCov [ Point c | (_, c, _) <- stations ]
                    (\p -> lookupElevation p)
  where
    elevations = zip (map (\(_, c, _) -> Point c) stations) [820, 810, 830, 140, 150, 990, 970, 50]
    lookupElevation p = maybe (error "no elevation on file") id (lookup p elevations)

main :: IO ()
main = do
  hSetEncoding stdout utf8

  putStrLn "== geojoin-demo: spatial join as a Coverage operation =="
  putStrLn ""
  putStrLn "8 rain-gauge stations (point + rainfall reading, mm):"
  mapM_ (\(sid, Point c, v) -> putStrLn ("  " ++ sid ++ " @ " ++ show c ++ ": " ++ show v ++ "mm"))
        [ (sid, Point c, v) | (sid, c, v) <- stations ]

  putStrLn ""
  putStrLn "3 municipalities (zones A, B, C — see source for the exact polygons)."

  putStrLn ""
  putStrLn "== Spatial join: mean rainfall per zone =="
  putStrLn "(select stations with pointInPolygon, then compose with mean — or both at once via `spatial`)"
  let mean xs = if null xs then 0 else sum xs / fromIntegral (length xs)
      countOf xs = fromIntegral (length xs) :: Double

      joinedMean  = spatial mean  rainfall pointInPolygon zones
      joinedCount = spatial countOf rainfall pointInPolygon zones
      joinedSum   = spatial sum   rainfall pointInPolygon zones

  mapM_ putStrLn
    [ "  zone " ++ name ++ ": " ++ show (round n :: Int) ++ " station(s), "
        ++ "sum = " ++ show s ++ "mm, mean = " ++ show m ++ "mm"
    | (name, n, s, m) <- zip4 zoneNames (values joinedCount) (values joinedSum) (values joinedMean)
    ]

  putStrLn ""
  putStrLn "  (station s8 falls outside every zone, so it never enters any of these"
  putStrLn "   sums/means/counts — a spatial join naturally discards the unmatched rows.)"

  putStrLn ""
  putStrLn "== Contrast: a non-spatial join (`multiple`), matching by point identity =="
  putStrLn "(joins rainfall + elevation station by station — no geometry test involved)"
  let combined = multiple (\vs -> case vs of
                              [r, e] -> "rainfall=" ++ show r ++ "mm, elevation=" ++ show e ++ "m"
                              _      -> "incomplete record")
                           [rainfall, elevation]
                           rainfall
  mapM_ putStrLn
    [ "  " ++ sid ++ ": " ++ label
    | ((sid, _, _), label) <- zip stations (values combined)
    ]

  putStrLn ""
  putStrLn "== Sanity checks (hand-computed expectations) =="
  let checks =
        [ ("zone A: 3 stations, sum 360mm, mean 120mm",
            zip3 (values joinedCount) (values joinedSum) (values joinedMean) !! 0 == (3, 360, 120))
        , ("zone B: 2 stations, sum 380mm, mean 190mm",
            zip3 (values joinedCount) (values joinedSum) (values joinedMean) !! 1 == (2, 380, 190))
        , ("zone C: 2 stations, sum 140mm, mean 70mm",
            zip3 (values joinedCount) (values joinedSum) (values joinedMean) !! 2 == (2, 140, 70))
        , ("s8 (outside every zone) is excluded from every zone's join",
            all (\z -> Point (Coord 10 10) `notElem` domain (select rainfall pointInPolygon z)) [zoneA, zoneB, zoneC])
        ]
  mapM_ (\(name, ok) -> putStrLn ("  [" ++ (if ok then "OK" else "FAIL") ++ "] " ++ name)) checks
  if all snd checks
    then putStrLn "\nAll checks passed."
    else error "geojoin-demo: a sanity check failed"

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a, b, c, d) : zip4 as bs cs ds
zip4 _ _ _ _ = []
