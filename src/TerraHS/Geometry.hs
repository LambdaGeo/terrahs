-- | The 'Geometry' type class, unifying operations common to every
-- supported shape.
--
-- The original TerraHS (built on TerraLib) had a separate family of
-- free functions per geometry type (@TeGeometryAlgorithms@). Here we
-- use a type class instead: explicit ad-hoc polymorphism, with no
-- dispatch hidden behind C++ FFI and no need for a "generic geometry"
-- type underneath.
module TerraHS.Geometry
  ( Geometry (..)
  , module TerraHS.Geometry.Coord
  , module TerraHS.Geometry.Point
  , module TerraHS.Geometry.Line
  , module TerraHS.Geometry.Polygon
  , module TerraHS.Geometry.BBox
  , module TerraHS.Geometry.Any
  ) where

import TerraHS.Geometry.Coord
import TerraHS.Geometry.Point
import TerraHS.Geometry.Line
import TerraHS.Geometry.Polygon
import TerraHS.Geometry.BBox
import TerraHS.Geometry.Any

-- | Operations common to any geometry.
class Geometry a where
  -- | The area of the geometry. Zero for points and lines.
  area :: a -> Double

  -- | Perimeter (lines and polygons) or length (lines). Zero for
  -- points.
  perimeter :: a -> Double

  -- | The centroid (center of mass) of the geometry.
  centroid :: a -> Coord

  -- | The bounding box of the geometry.
  envelope :: a -> BBox

instance Geometry Point where
  area _          = 0
  perimeter _     = 0
  centroid        = pointCoord
  envelope (Point c) = BBox (coordX c) (coordY c) (coordX c) (coordY c)

instance Geometry Line where
  area _          = 0
  perimeter       = lineLength
  centroid (Line cs) = averageCoords cs
  envelope l@(Line cs) =
    maybe (error boundsError) id (fromCoords cs)
    where boundsError = "envelope: Line with no coordinates — the mkLine invariant was violated: " ++ show l

instance Geometry Polygon where
  area      = polygonArea
  perimeter = polygonPerimeter
  centroid  = polygonCentroid
  envelope p@(Polygon ring) =
    maybe (error boundsError) id (fromCoords ring)
    where boundsError = "envelope: Polygon with no coordinates — the mkPolygon invariant was violated: " ++ show p

-- | Delegates to the concrete case — the sole purpose of this type is
-- to allow file I/O with mixed geometries; the actual logic stays in
-- the 'Point', 'Line', and 'Polygon' instances.
instance Geometry AnyGeometry where
  area (AGPoint p)   = area p
  area (AGLine l)    = area l
  area (AGPolygon p) = area p

  perimeter (AGPoint p)   = perimeter p
  perimeter (AGLine l)    = perimeter l
  perimeter (AGPolygon p) = perimeter p

  centroid (AGPoint p)   = centroid p
  centroid (AGLine l)    = centroid l
  centroid (AGPolygon p) = centroid p

  envelope (AGPoint p)   = envelope p
  envelope (AGLine l)    = envelope l
  envelope (AGPolygon p) = envelope p

-- | Simple arithmetic mean of a non-empty list of coordinates. Used
-- as an approximation of a line's centroid (it does not weight by
-- segment length — good enough for teaching purposes, but worth
-- revisiting if precision matters for research use).
averageCoords :: [Coord] -> Coord
averageCoords cs = Coord (sumX / n) (sumY / n)
  where
    n    = fromIntegral (length cs)
    sumX = sum (map coordX cs)
    sumY = sum (map coordY cs)
