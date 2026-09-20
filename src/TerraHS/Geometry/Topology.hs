-- | Topological predicates between geometries.
--
-- Corresponds to the original @TeTopologyOps@. Most of it is still
-- bounding-box based ('BBox'). Exact up to floating-point tolerance:
--
-- * 'pointOnLine' — a point lying on a polyline, via point-to-segment
--   distance.
-- * 'pointInPolygon' — a point lying inside a simple polygon, via ray
--   casting (counting crossings of a horizontal ray — the Jordan
--   curve theorem).
-- * 'crossesPolygon' — a polyline crossing a polygon's boundary, via
--   segment-segment intersection against every edge (plus
--   'pointInPolygon' as a shortcut for a vertex that starts or ends
--   inside). A simplified generic-position test — it doesn't
--   special-case exact collinear overlap between a road segment and
--   a polygon edge, which a fully general-purpose version would need
--   to handle.
module TerraHS.Geometry.Topology
  ( intersects
  , contains
  , pointOnLine
  , distanceToLine
  , pointInPolygon
  , crossesPolygon
  , segmentsIntersect
  ) where

import TerraHS.Geometry.Coord (Coord (..), distance)
import TerraHS.Geometry.Point (Point (..))
import TerraHS.Geometry.Line (Line, lineCoords, lineSegments)
import TerraHS.Geometry.Polygon (Polygon (..))
import TerraHS.Geometry.BBox (BBox (..), bboxIntersects)
import TerraHS.Geometry (Geometry (..))

-- | Two geometries overlap if their bounding boxes overlap. A
-- conservative approximation (false positives possible, no false
-- negatives) — the same "broad phase" filter used by engines like
-- GEOS ahead of the exact test.
intersects :: (Geometry a, Geometry b) => a -> b -> Bool
intersects a b = bboxIntersects (envelope a) (envelope b)

-- | A bounding box contains a coordinate.
contains :: BBox -> Coord -> Bool
contains (BBox minX minY maxX maxY) (Coord x y) =
  x >= minX && x <= maxX && y >= minY && y <= maxY

-- | Distance from a point to the nearest segment — projects the
-- point onto each segment (clamping the parameter to [0,1] so it
-- doesn't "slide" past the endpoints) and takes the minimum.
distanceToLine :: Point -> Line -> Double
distanceToLine (Point p) l = minimum (map (distanceToSegment p) (lineSegments l))
  where
    distanceToSegment q (a, b) =
      let abx = coordX b - coordX a
          aby = coordY b - coordY a
          apx = coordX q - coordX a
          apy = coordY q - coordY a
          abLenSq = abx * abx + aby * aby
          t | abLenSq == 0 = 0
            | otherwise    = max 0 (min 1 ((apx * abx + apy * aby) / abLenSq))
          proj = Coord (coordX a + t * abx) (coordY a + t * aby)
      in distance q proj

-- | A point lies on the polyline if the distance to it is below a
-- tolerance (to accommodate floating-point imprecision — in exact
-- geometry the tolerance would be 0).
pointOnLine :: Double -> Point -> Line -> Bool
pointOnLine tolerance p l = distanceToLine p l <= tolerance

-- | A point lies inside a simple polygon (no holes), tested by ray
-- casting: counts how many edges of the ring a horizontal ray from
-- the point to the right crosses. An odd number of crossings means
-- inside; an even number means outside. Behaviour on the exact
-- boundary is not guaranteed (the classic floating-point corner case
-- of ray casting) — for points clearly inside or outside, it is
-- exact.
pointInPolygon :: Point -> Polygon -> Bool
pointInPolygon (Point (Coord px py)) (Polygon ring) =
  odd (length (filter crossesRay (zip ring (drop 1 ring))))
  where
    crossesRay (Coord x1 y1, Coord x2 y2) =
      ((y1 > py) /= (y2 > py))
        && (px < (x2 - x1) * (py - y1) / (y2 - y1) + x1)

-- | Twice the signed area of the triangle (p, q, r) — its sign gives
-- the turn direction (>0 counter-clockwise, <0 clockwise, 0
-- collinear). The standard building block for segment intersection.
orientation :: Coord -> Coord -> Coord -> Double
orientation (Coord px py) (Coord qx qy) (Coord rx ry) =
  (qx - px) * (ry - py) - (qy - py) * (rx - px)

-- | Do two segments cross? A simplified generic-position test (via
-- 'orientation' sign changes) — see the module haddock's note on
-- what it doesn't handle.
segmentsIntersect :: (Coord, Coord) -> (Coord, Coord) -> Bool
segmentsIntersect (p1, p2) (p3, p4) =
  let o1 = orientation p1 p2 p3
      o2 = orientation p1 p2 p4
      o3 = orientation p3 p4 p1
      o4 = orientation p3 p4 p2
  in (signum o1 /= signum o2) && (signum o3 /= signum o4)

-- | A polyline crosses a polygon if any of its segments crosses one
-- of the polygon's edges, or if any of its vertices lies inside the
-- polygon (covers the case of a segment starting or ending inside,
-- rather than passing all the way through).
crossesPolygon :: Line -> Polygon -> Bool
crossesPolygon line poly =
  any (\v -> pointInPolygon (Point v) poly) (lineCoords line)
    || any (\seg -> any (segmentsIntersect seg) polyEdges) (lineSegments line)
  where
    polyEdges = zip (polygonRing poly) (drop 1 (polygonRing poly))
