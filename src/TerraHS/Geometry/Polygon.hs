-- | Simple polygon geometry (no holes, for now).
--
-- Corresponds to the original @TePolygon@.
module TerraHS.Geometry.Polygon
  ( Polygon (..)
  , mkPolygon
  , polygonArea
  , polygonPerimeter
  , polygonCentroid
  ) where

import TerraHS.Geometry.Coord (Coord (..), distance)

-- | A polygon: a closed ring of coordinates (the first and last
-- points coincide). The constructor is not exported — use
-- 'mkPolygon' to enforce that invariant and a minimum of 3 distinct
-- vertices.
newtype Polygon = Polygon { polygonRing :: [Coord] }
  deriving (Eq, Show)

-- | Builds a 'Polygon', automatically closing the ring if needed.
-- Returns 'Nothing' if there are fewer than 3 distinct vertices.
mkPolygon :: [Coord] -> Maybe Polygon
mkPolygon cs =
  case dedupClose cs of
    ring | length ring >= 4 -> Just (Polygon ring) -- closed: N distinct points + repeated first
    _                       -> Nothing
  where
    dedupClose [] = []
    dedupClose xs@(first : _)
      | last xs == first = xs
      | otherwise         = xs ++ [first]

-- | Polygon area via the shoelace (Gauss) formula.
polygonArea :: Polygon -> Double
polygonArea (Polygon ring) =
  abs (sum (zipWith cross ring (drop 1 ring))) / 2
  where
    cross (Coord x1 y1) (Coord x2 y2) = x1 * y2 - x2 * y1

-- | Polygon perimeter: sum of the ring's edges.
polygonPerimeter :: Polygon -> Double
polygonPerimeter (Polygon ring) =
  sum (zipWith distance ring (drop 1 ring))

-- | Polygon centroid (center of mass).
polygonCentroid :: Polygon -> Coord
polygonCentroid (Polygon ring) = Coord (cx / (6 * a)) (cy / (6 * a))
  where
    a  = sum (zipWith cross ring (drop 1 ring)) / 2
    cx = sum (zipWith termX ring (drop 1 ring))
    cy = sum (zipWith termY ring (drop 1 ring))
    cross (Coord x1 y1) (Coord x2 y2) = x1 * y2 - x2 * y1
    termX (Coord x1 y1) (Coord x2 y2) = (x1 + x2) * (x1 * y2 - x2 * y1)
    termY (Coord x1 y1) (Coord x2 y2) = (y1 + y2) * (x1 * y2 - x2 * y1)
