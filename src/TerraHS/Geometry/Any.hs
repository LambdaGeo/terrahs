-- | A geometry whose concrete type is only known at run time —
-- needed for file I/O (WKT, GeoJSON, Shapefile), since a file can
-- contain a mix of points, lines, and polygons.
module TerraHS.Geometry.Any
  ( AnyGeometry (..)
  ) where

import TerraHS.Geometry.Point (Point)
import TerraHS.Geometry.Line (Line)
import TerraHS.Geometry.Polygon (Polygon)

data AnyGeometry
  = AGPoint   Point
  | AGLine    Line
  | AGPolygon Polygon
  deriving (Eq, Show)
