-- | Line and polygon simplification: the Ramer-Douglas-Peucker
-- algorithm.
--
-- A classic recursive, divide-and-conquer geometry algorithm (Douglas
-- and Peucker, 1973; independently, Ramer, 1972): given a tolerance
-- @epsilon@, it drops every vertex that lies within @epsilon@ of the
-- straight line already implied by its neighbours, keeping only the
-- vertices that actually bend the shape by more than that tolerance.
-- The classic use is exactly the one here -- a polygon digitized (or
-- surveyed) at far more detail than a given map or screen will ever
-- show benefits from being simplified /to that scale/ before it's
-- drawn, since detail below the pixel size is invisible work.
--
-- Purely arithmetic on coordinates -- no new dependency, unlike
-- rendering ('TerraHS.Render.PNG', its own library component). Lives
-- in the core @terrahs@ library because it's a geometry operation
-- like any other in "TerraHS.Geometry.Topology", not tied to
-- rendering at all: a simplified layer is smaller to store, faster to
-- test topological predicates against, and faster to render, in that
-- order of generality.
module TerraHS.Geometry.Simplify
  ( simplifyCoords
  , simplifyLine
  , simplifyPolygon
  ) where

import Data.List (maximumBy)
import Data.Ord (comparing)

import TerraHS.Geometry.Coord (Coord (..), distance)
import TerraHS.Geometry.Line (Line, lineCoords, mkLine)
import TerraHS.Geometry.Polygon (Polygon (..), mkPolygon)

-- | The perpendicular distance from a point to the (infinite) line
-- through two others -- falls back to plain point-to-point distance
-- when those two coincide (a zero-length baseline has no direction to
-- be perpendicular to).
perpendicularDistance :: Coord -> Coord -> Coord -> Double
perpendicularDistance (Coord x1 y1) (Coord x2 y2) (Coord x0 y0)
  | x1 == x2 && y1 == y2 = distance (Coord x1 y1) (Coord x0 y0)
  | otherwise = abs ((y2 - y1) * x0 - (x2 - x1) * y0 + x2 * y1 - y2 * x1) / baseLen
  where
    baseLen = sqrt ((y2 - y1) ^ (2 :: Int) + (x2 - x1) ^ (2 :: Int))

-- | Simplifies an open chain of coordinates: the first and last point
-- are always kept; find the point farthest (perpendicularly) from the
-- straight line between them; if that farthest distance exceeds
-- @epsilon@, split there and simplify each half independently (it
-- bends the shape too much to drop); otherwise collapse the whole
-- chain to just its two endpoints (everything between was within
-- @epsilon@ of the straight line already).
--
-- >>> simplifyCoords 0.5 [Coord 0 0, Coord 1 0.1, Coord 2 0]
-- [Coord 0 0,Coord 2 0]
simplifyCoords :: Double -> [Coord] -> [Coord]
simplifyCoords epsilon coords
  | length coords < 3 = coords
  | farthestDist > epsilon =
      simplifyCoords epsilon before ++ drop 1 (simplifyCoords epsilon after)
  | otherwise = [first, end]
  where
    first = head coords
    end   = last coords
    (farthestIdx, farthestDist) =
      maximumBy (comparing snd) (zip [0 :: Int ..] (map (perpendicularDistance first end) coords))
    -- Both halves must include the split point itself (as the end of
    -- 'before' and the start of 'after'), or it's lost from the
    -- result entirely -- so this isn't a single 'splitAt' pair, the
    -- two halves overlap by that one shared element.
    before = take (farthestIdx + 1) coords
    after  = drop farthestIdx coords

-- | Simplifies a 'Line' -- an open polyline, so 'simplifyCoords'
-- applies directly.
simplifyLine :: Double -> Line -> Line
simplifyLine epsilon l = maybe l id (mkLine (simplifyCoords epsilon (lineCoords l)))

-- | Simplifies a 'Polygon''s ring. A closed ring (first point == last)
-- needs a small adaptation of the plain algorithm: using the shared
-- first\/last point as the baseline would degrade 'perpendicularDistance'
-- to a plain point-to-point distance (a zero-length baseline has no
-- direction), simplifying poorly. The standard fix: pick the vertex
-- farthest from the first point as a second anchor, splitting the
-- ring into two open chains at those two anchors, simplify each
-- independently, then rejoin.
simplifyPolygon :: Double -> Polygon -> Polygon
simplifyPolygon epsilon poly@(Polygon ring)
  | length openRing < 3 = poly
  | otherwise           = maybe poly id (mkPolygon simplified)
  where
    openRing = init ring -- drop the duplicated closing point
    anchor0  = head openRing
    (anchorIdx, _) =
      maximumBy (comparing snd) (zip [0 :: Int ..] (map (distance anchor0) openRing))
    chainA = take (anchorIdx + 1) openRing       -- anchor0 .. anchor1
    chainB = drop anchorIdx openRing ++ [anchor0] -- anchor1 .. anchor0 (wrapping)
    simplified = simplifyCoords epsilon chainA ++ drop 1 (simplifyCoords epsilon chainB)
