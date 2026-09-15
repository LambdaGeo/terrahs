-- | Point geometry.
--
-- Corresponds to the original @TePoint@.
module TerraHS.Geometry.Point
  ( Point (..)
  ) where

import TerraHS.Geometry.Coord (Coord)

-- | A point geometry, wrapping a single coordinate.
newtype Point = Point { pointCoord :: Coord }
  deriving (Eq, Ord, Show)
