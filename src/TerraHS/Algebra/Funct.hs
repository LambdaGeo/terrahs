-- | The 'Funct' class: the core of the map algebra from the original
-- 2005-2009 TerraHS codebase.
--
-- The central idea: instead of defining separate operators for
-- "summing two rasters", "multiplying a raster by a scalar",
-- "applying a function to each cell", and so on, all of these
-- operators are cases of a single operation — "lifting" a pure
-- function of N arguments into a container @f@, applying it position
-- by position. This is essentially the 'Functor'\/'Applicative'
-- family, generalized beyond arity 1 and named as in the original
-- code.
--
-- In the 2005-2009 code (@TerraHS.Algebras.Base.Category@), the list
-- instance defined 'lift0'\/'lift1'\/'lift2'\/'lift3', and the
-- @TeRaster@ instance (in @TerraHS.TerraLib.TeRaster@, version 0.9)
-- only defined 'lift1'\/'lift2' — the comments already pointed at
-- @lift3@\/@lift4@ as the destination for 3x3 convolution operators,
-- but that was never implemented. This module faithfully recovers the
-- class; 'TerraHS.Algebra.Field' fills in the part that was left as a
-- comment — together, the two implement Tomlin's (1990) classic
-- taxonomy: local ('lift1'\/'lift2'), focal ('focal3x3'), zonal
-- ('TerraHS.Algebra.Field.zonalWith'), and global
-- ('TerraHS.Algebra.Field.sumField' and friends).
module TerraHS.Algebra.Funct
  ( Funct (..)
  ) where

class Funct f where
  -- | "Flattens" a single-element container back to the value. No
  -- default implementation — not every 'Funct' can do this without
  -- more context (e.g. a 'TerraHS.Algebra.Field.Field' of arbitrary
  -- dimensions has no way to "flatten" to a scalar without an
  -- aggregation rule — see 'TerraHS.Algebra.Field.foldField' for
  -- that).
  unlift :: f a -> a
  unlift _ = error "Funct: unlift not implemented for this functor"

  -- | Lifts a constant value into the container (equivalent to
  -- 'pure'\/'return' from 'Applicative'). Also without a default
  -- implementation, for the same reason as 'unlift': a container with
  -- shape/dimensions (like 'TerraHS.Algebra.Field.Field') needs to
  -- know which shape to use.
  lift0 :: a -> f a
  lift0 _ = error "Funct: lift0 not implemented for this functor"

  -- | Applies a unary function position by position — the classic
  -- LOCAL operator of map algebra, e.g. doubling every value of a
  -- field (@lift1 (*2) field@). Every instance of 'Funct' in this
  -- module implements this.
  lift1 :: (a -> b) -> f a -> f b

  -- | Combines two containers position by position with a binary
  -- function — also a LOCAL operator, e.g. summing two fields
  -- (@lift2 (+) field1 field2@). Every instance of 'Funct' in this
  -- module implements this.
  lift2 :: (a -> b -> c) -> f a -> f b -> f c

  -- | Combines three containers position by position. Only the
  -- original list instance implemented this.
  lift3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
  lift3 _ _ _ _ = error "Funct: lift3 not implemented for this functor"

  -- | Combines four containers position by position. Never
  -- implemented by any instance in the original code.
  lift4 :: (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
  lift4 _ _ _ _ _ = error "Funct: lift4 not implemented for this functor"

  -- | 'Applicative'-style application (@<*>@), derived from 'lift2' —
  -- the original code never defined this, but it comes for free from
  -- 'lift2' for any instance, so it's worth having.
  ($*$) :: f (a -> b) -> f a -> f b
  ($*$) = lift2 ($)

infixl 4 $*$

-- | The original instance: lists, with 'lift1' and 'lift2' equivalent
-- to 'map' and 'zipWith', and 'lift3' generalizing to three lists.
instance Funct [] where
  unlift [a] = a
  unlift xs  = error ("Funct []: unlift expects a single-element list, got " ++ show (length xs))

  lift0 a = [a]

  lift1 = map

  lift2 = zipWith

  lift3 f (a : as) (b : bs) (c : cs) = f a b c : lift3 f as bs cs
  lift3 _ _ _ _ = []
