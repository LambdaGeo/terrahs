-- | The six-zone synthetic polygon domain shared by @diffusion-demo@
-- and @fire-demo@ -- two different rules running over the exact same
-- world, so the world itself is factored out here rather than
-- duplicated.
module ZoneWorld
  ( Zone (..)
  , zones
  , adjacent
  , canvasBBox
  , zoneCoverage
  ) where

import Control.Comonad.Store (Store, peek)
import Data.Functor.Contravariant (Predicate (..))

import TerraHS
import TerraHS.CA (notSelf, touchesVia)

-- | A named zone: an id (for display and comparison) and the polygon
-- it stands for. Adjacency is decided from the polygon; identity
-- (for 'Store' positioning and the "not myself" check) from the id
-- alone, which is why 'Eq' is written by hand rather than derived.
data Zone = Zone { zoneId :: String, zonePoly :: Polygon }

instance Eq Zone where
  a == b = zoneId a == zoneId b

instance Show Zone where
  show = zoneId

sq :: String -> (Double, Double) -> (Double, Double) -> Zone
sq zid (x0, y0) (x1, y1) =
  Zone zid (maybe (error "invariant violated") id
              (mkPolygon [Coord x0 y0, Coord x1 y0, Coord x1 y1, Coord x0 y1]))

-- | Six unit squares laid out as a sparse, hand-traceable adjacency
-- graph (a chain with one side-branch) -- deliberately synthetic
-- rather than reusing the real IBGE municipalities: those four real
-- islands sit close enough together that every pair's bounding box
-- overlaps, so the whole set would "infect" in a single step. These
-- squares only share edges where drawn to, so a seed at one end
-- spreads gradually, matching the paper's own figure (diffusion
-- advancing t1 -> t2 -> t3 -> t4, not all at once):
--
-- >   Z5
-- >   Z4
-- > Z1 Z2 Z3 Z6
--
-- 'intersects' is bounding-box based (see 'TerraHS.Geometry.Topology')
-- with inclusive comparisons on each axis independently, so it treats
-- two squares that only meet at a shared corner as adjacent too, not
-- only ones that share a full edge -- worth knowing when reading the
-- spread each demo checks against. That gives: Z1-Z2 (edge x=1),
-- Z2-Z3 (edge x=2), Z3-Z6 (edge x=3), Z2-Z4 (edge y=1), Z4-Z5
-- (edge y=2), plus two corner touches, Z1-Z4 (at (1,1)) and Z3-Z4 (at
-- (2,1)) -- every other pair is disjoint. So each zone's neighbours
-- are: Z1={Z2,Z4}, Z2={Z1,Z3,Z4}, Z3={Z2,Z4,Z6}, Z4={Z1,Z2,Z3,Z5},
-- Z5={Z4}, Z6={Z3}.
zones :: [Zone]
zones =
  [ sq "Z1" (0, 0) (1, 1)
  , sq "Z2" (1, 0) (2, 1)
  , sq "Z3" (2, 0) (3, 1)
  , sq "Z4" (1, 1) (2, 2)
  , sq "Z5" (1, 2) (2, 3)
  , sq "Z6" (3, 0) (4, 1)
  ]

-- | Adjacency: two distinct zones whose polygons touch (edge or
-- corner). Built from 'TerraHS.CA''s generic 'notSelf' and
-- 'touchesVia' -- exactly the composition the source paper asked for
-- when it wanted to combine a spatial test with a non-spatial one,
-- via 'Predicate''s 'Monoid' instance ('<>' is logical AND). Shared
-- by both models built on 'zones' (diffusion and fire) -- "adjacent"
-- means the same thing to both.
adjacent :: Predicate (Zone, Zone)
adjacent = notSelf <> touchesVia zonePoly intersects

-- | The smallest box covering every zone, for sizing the PNG canvas
-- ('TerraHS.Render.PNG.renderCoverage' takes this explicitly so every
-- frame of a run shares the same canvas and lines up).
canvasBBox :: BBox
canvasBBox = foldr1 union (map (envelope . zonePoly) zones)

-- | A 'Store' over 'Zone' turned into a plain @Coverage Polygon v@,
-- ready for 'TerraHS.Render.PNG' (which only knows about 'Coverage',
-- not about 'Zone'). The one piece of real duplication the original,
-- unsplit @comonad-ca-demo@ had -- the diffusion and fire models each
-- wrote this out by hand, identical but for the value type -- factored
-- into a single helper both demos now share.
zoneCoverage :: Store Zone v -> Coverage Polygon v
zoneCoverage w = fromPairs [ (zonePoly z, peek z w) | z <- zones ]
