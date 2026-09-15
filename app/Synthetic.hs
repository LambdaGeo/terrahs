-- | Reproducible synthetic data generation (fixed seed) for the demo
-- executable — only used by @app/Main.hs@, not part of the library
-- (which is why the @random@ dependency stays isolated in the
-- executable, without weighing down the library itself).
module Synthetic
  ( syntheticField
  , syntheticPoints
  ) where

import System.Random (mkStdGen, randomRs, split)

import TerraHS (Coord (..), Field, Point (..), mkField)

-- | A @rows@ x @cols@ field of pseudo-random values in [0,1),
-- reproducible from an integer seed — the same seed always produces
-- the same field.
syntheticField :: Int -> Int -> Int -> Field Double
syntheticField seed rows cols =
  case mkField rowsOfValues of
    Just f  -> f
    Nothing -> error "syntheticField: invalid dimensions (should not happen)"
  where
    allValues   = take (rows * cols) (randomRs (0, 1) (mkStdGen seed))
    rowsOfValues = chunksOf cols allValues
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | @n@ random points inside a bounding box
-- @(minX, minY, maxX, maxY)@, each paired with a value also random
-- in [0,1) (meant to represent something like a deforestation
-- percentage). Reproducible from a seed.
syntheticPoints :: Int -> Int -> (Double, Double, Double, Double) -> [(Point, Double)]
syntheticPoints seed n (minX, minY, maxX, maxY) = take n (zip3' xs ys vs)
  where
    g0       = mkStdGen seed
    (g1, g') = split g0
    (g2, g3) = split g'
    xs = randomRs (minX, maxX) g1
    ys = randomRs (minY, maxY) g2
    vs = randomRs (0, 1) g3

    zip3' (x : xs') (y : ys') (v : vs') = (Point (Coord x y), v) : zip3' xs' ys' vs'
    zip3' _ _ _ = []
