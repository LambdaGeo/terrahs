module Main (main) where

import TerraHS
import Synthetic (syntheticField, syntheticPoints)
import System.IO (hSetEncoding, stdout, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  putStrLn "== Geometry =="
  let Just square = mkPolygon [Coord 0 0, Coord 4 0, Coord 4 4, Coord 0 4]
  putStrLn ("area of the 4x4 square: " ++ show (area square))
  putStrLn ("perimeter: "              ++ show (perimeter square))
  putStrLn ("centroid: "               ++ show (centroid square))

  putStrLn ""
  putStrLn "== WKT =="
  putStrLn (renderWKT (AGPolygon square))

  putStrLn ""
  putStrLn "== Map algebra — Coverage (the thesis algebra) =="
  -- A small-scale reconstruction of the deforestation classification
  -- example from Chapter 4: single applies a single-argument function
  -- to every value of the coverage.
  let deforestation = newCov [1, 2, 3, 4 :: Int]
        (\x -> [0.1, 0.35, 0.65, 0.9 :: Double] !! (x - 1))
      classify v
        | v < 0.2   = "dense forest"
        | v < 0.5   = "mixed forest with agriculture"
        | v < 0.8   = "agriculture with forest fragments"
        | otherwise = "agricultural area"
      classified = single classify deforestation
  mapM_ putStrLn
    [ "  cell " ++ show cellId ++ " (" ++ show v ++ "): " ++ label
    | (cellId, v, label) <- zip3 (domain deforestation) (values deforestation) (values classified)
    ]

  putStrLn ""
  putStrLn "== Map algebra — Field (grid, focal/global operators) =="
  let Just field = mkField [[1, 2, 3], [4, 5, 6], [7, 8, 9 :: Double]]
      focalMean  = focal3x3 0 (\ns -> sum ns / fromIntegral (length ns)) field
  putStrLn ("total sum (global operator): " ++ show (sumField field))
  putStrLn ("3x3 focal mean at the center cell: " ++ show (fieldAt focalMean 1 1))

  putStrLn ""
  putStrLn "== Synthetic data: a larger field (8x8, fixed seed) =="
  let bigField = syntheticField 42 8 8
  putStrLn ("global mean of the 8x8 field: " ++ show (meanField bigField))
  putStrLn ("largest value: " ++ show (maxField bigField) ++ ", smallest: " ++ show (minField bigField))

  putStrLn ""
  putStrLn "== Synthetic data: reconstructing the Pará example (Fig. 4.21-4.24) =="
  putStrLn "(30 random \"deforestation\" points in a 0..10 x 0..10 area,"
  putStrLn " 2 fixed \"protection areas\" and 1 fixed \"road\" crossing through them)"
  let pts       = syntheticPoints 7 30 (0, 0, 10, 10)
      pointsCov = newCov (map fst pts) (\p -> maybe 0 id (lookup p pts))

      Just protArea1 = mkPolygon [Coord 1 1, Coord 4 1, Coord 4 4, Coord 1 4]
      Just protArea2 = mkPolygon [Coord 6 6, Coord 9 6, Coord 9 9, Coord 6 9]
      protAreas      = [protArea1, protArea2]
      protAreasCov   = newCov protAreas (const (0 :: Double))

      Just road = mkLine [Coord 0 0, Coord 5 3, Coord 10 5]
      roadCov   = newCov [road] (const (0 :: Double))
      roadTolerance = 0.5

      meanByArea = spatial mean pointsCov pointInPolygon protAreasCov
      meanAlongRoad = spatial mean pointsCov (pointOnLine roadTolerance) roadCov
      mean xs = if null xs then 0 else sum xs / fromIntegral (length xs)

  mapM_ putStrLn
    [ "  protection area " ++ show i ++ ": mean = " ++ show v
        ++ " (" ++ show (length (domain (select pointsCov pointInPolygon region))) ++ " points inside)"
    | (i, region, v) <- zip3 [1 :: Int ..] protAreas (values meanByArea)
    ]
  putStrLn
    ( "  along the road (tolerance " ++ show roadTolerance ++ "): mean = "
      ++ show (head (values meanAlongRoad))
      ++ " (" ++ show (length (domain (select pointsCov (pointOnLine roadTolerance) road))) ++ " nearby points)"
    )
