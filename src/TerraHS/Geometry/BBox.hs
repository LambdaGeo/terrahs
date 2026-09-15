-- | Axis-aligned bounding box.
--
-- Corresponds to the original @TeBox@. The name follows common
-- GEOS/GDAL usage ("envelope"), abbreviated here to 'BBox'.
module TerraHS.Geometry.BBox
  ( BBox (..)
  , fromCoords
  , union
  , bboxIntersects
  ) where

import TerraHS.Geometry.Coord (Coord (..))

-- | An axis-aligned bounding box.
data BBox = BBox
  { bboxMinX :: !Double
  , bboxMinY :: !Double
  , bboxMaxX :: !Double
  , bboxMaxY :: !Double
  } deriving (Eq, Show)

-- | Builds the smallest 'BBox' that contains all the given
-- coordinates. Returns 'Nothing' for an empty list.
fromCoords :: [Coord] -> Maybe BBox
fromCoords [] = Nothing
fromCoords (c : cs) =
  Just (foldr grow (BBox (coordX c) (coordY c) (coordX c) (coordY c)) cs)
  where
    grow (Coord x y) (BBox minX minY maxX maxY) =
      BBox (min minX x) (min minY y) (max maxX x) (max maxY y)

-- | The smallest 'BBox' that contains two others.
union :: BBox -> BBox -> BBox
union (BBox minX1 minY1 maxX1 maxY1) (BBox minX2 minY2 maxX2 maxY2) =
  BBox (min minX1 minX2) (min minY1 minY2) (max maxX1 maxX2) (max maxY1 maxY2)

-- | A quick overlap test between two bounding boxes. Useful as a
-- cheap filter before a more expensive exact topological test.
bboxIntersects :: BBox -> BBox -> Bool
bboxIntersects (BBox minX1 minY1 maxX1 maxY1) (BBox minX2 minY2 maxX2 maxY2) =
  minX1 <= maxX2 && maxX1 >= minX2 && minY1 <= maxY2 && maxY1 >= minY2
