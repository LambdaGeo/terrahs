-- | 2D Cartesian coordinates.
--
-- Corresponds to the original @TeCoord2D@ from TerraLib, now as a
-- pure type with no external dependencies.
module TerraHS.Geometry.Coord
  ( Coord (..)
  , distance
  , translate
  ) where

-- | A coordinate on the Cartesian plane.
data Coord = Coord
  { coordX :: !Double
  , coordY :: !Double
  } deriving (Eq, Ord, Show)

-- | Euclidean distance between two coordinates.
distance :: Coord -> Coord -> Double
distance (Coord x1 y1) (Coord x2 y2) =
  sqrt ((x2 - x1) ^ (2 :: Int) + (y2 - y1) ^ (2 :: Int))

-- | Translates a coordinate by an offset (dx, dy).
translate :: Double -> Double -> Coord -> Coord
translate dx dy (Coord x y) = Coord (x + dx) (y + dy)
