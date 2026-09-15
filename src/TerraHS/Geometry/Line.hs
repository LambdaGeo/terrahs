-- | Line geometry (open polyline).
--
-- Corresponds to the original @TeLine2D@.
module TerraHS.Geometry.Line
  ( Line (..)
  , mkLine
  , lineLength
  , lineSegments
  ) where

import TerraHS.Geometry.Coord (Coord, distance)

-- | A polyline: an ordered sequence of coordinates with at least two
-- points. The constructor is not exported — use 'mkLine' to enforce
-- this invariant.
newtype Line = Line { lineCoords :: [Coord] }
  deriving (Eq, Show)

-- | Builds a 'Line' from a list of coordinates. Returns 'Nothing' if
-- the list has fewer than two points.
mkLine :: [Coord] -> Maybe Line
mkLine cs
  | length cs >= 2 = Just (Line cs)
  | otherwise      = Nothing

-- | The consecutive segments that make up the line.
lineSegments :: Line -> [(Coord, Coord)]
lineSegments (Line cs) = zip cs (drop 1 cs)

-- | Total length of the line (sum of the segments).
lineLength :: Line -> Double
lineLength = sum . map (uncurry distance) . lineSegments
