-- | A tiny, generic cellular-automaton machine: neighbourhoods and
-- time stepping, factored out of the pattern @comonad-ca-demo@'s
-- Game of Life and diffusion models both hand-rolled independently.
-- Plays the same role as a @CellularAutomaton@ base class in an
-- object-oriented framework (setup a neighbourhood, seed an initial
-- state, supply a per-cell transition rule) -- except there is no
-- class to subclass: a model here is just a domain, an adjacency
-- 'Predicate', and a rule function, combined with plain values.
--
-- Its own library component (@terrahs-ca@ in @terrahs.cabal@),
-- alongside @terrahs-render@ -- kept out of the core @terrahs@
-- library for the same reason: this depends on @comonad@ and
-- @contravariant@, real dependencies that reading a shapefile or
-- running the map algebra has no reason to carry. Anything that
-- wants to build a cellular automaton (a new example, say) depends
-- on @terrahs-ca@ explicitly instead.
module TerraHS.CA
  ( -- * Neighbourhoods
    neighborValues
  , countNeighbors
    -- * Building adjacency predicates
  , notSelf
  , touchesVia
    -- * Unbounded integer grids
  , moore8
    -- * Stepping
  , stepCA
  , runCA
  , seedCA
  ) where

import Control.Comonad (extend)
import Control.Comonad.Store (Store, store, pos, peek)
import Data.Functor.Contravariant (Predicate (..))

-- | The values of every cell adjacent to the one a 'Store' is
-- currently focused on, per an adjacency 'Predicate' -- the
-- equivalent of a Python framework's @self.neighbor_values(idx,
-- attr)@, just without an object to call it on. @universe@ is the
-- whole domain to search for neighbours in (the same list every rule
-- in a model shares, typically).
neighborValues :: Eq e => Predicate (e, e) -> [e] -> Store e s -> [s]
neighborValues adjacent universe w =
  [ peek e w | e <- universe, getPredicate adjacent (pos w, e) ]

-- | How many neighbours currently satisfy a value predicate -- e.g.
-- "how many neighbours are burning". A thin convenience over
-- 'neighborValues' for rules that only care about a count (Life's
-- B3/S23, say).
countNeighbors :: Eq e => Predicate (e, e) -> [e] -> (s -> Bool) -> Store e s -> Int
countNeighbors adjacent universe isOfInterest w =
  length (filter isOfInterest (neighborValues adjacent universe w))

-- | Excludes a domain element from counting as its own neighbour --
-- combine with a spatial (or any other) adjacency test via
-- 'Predicate''s 'Monoid' instance ('<>' is logical AND) to build the
-- adjacency predicate a model needs, e.g.
-- @notSelf \<\> touchesVia zonePoly intersects@. Generic over any
-- 'Eq' domain, not tied to any particular model.
notSelf :: Eq e => Predicate (e, e)
notSelf = Predicate (\(a, b) -> a /= b)

-- | Lifts a binary relation on some value projected out of the
-- domain -- typically a spatial predicate like
-- 'TerraHS.Geometry.Topology.intersects', projected via a field like
-- @zonePoly@ -- into a 'Predicate' over pairs of domain elements.
-- Doesn't depend on 'TerraHS.Geometry' itself (the relation is passed
-- in, not imported), keeping @terrahs-ca@ free of a dependency on the
-- core @terrahs@ library.
touchesVia :: (e -> g) -> (g -> g -> Bool) -> Predicate (e, e)
touchesVia project rel = Predicate (\(a, b) -> rel (project a) (project b))

-- | One step of a cellular automaton: the transition rule, applied to
-- every cell of the world at once via 'Control.Comonad.extend'. The
-- rule itself decides what "neighbourhood" means for that model,
-- typically by calling 'neighborValues' or 'countNeighbors' with
-- whatever adjacency 'Predicate' the model was set up with.
stepCA :: (Store e s -> s) -> Store e s -> Store e s
stepCA = extend

-- | Runs a cellular automaton from a seed, as the (lazy, infinite)
-- sequence of worlds it passes through -- 'take' as many as you want
-- to look at. The direct replacement for a hand-written simulation
-- loop.
runCA :: (Store e s -> s) -> Store e s -> [Store e s]
runCA rule = iterate (stepCA rule)

-- | Builds the initial world: an initial-state function over the
-- domain, focused at a starting position (which cell it starts
-- focused on rarely matters -- every cell's state is already fixed by
-- @initial@ -- but 'Store' always needs one).
seedCA :: (e -> s) -> e -> Store e s
seedCA = store

-- | The eight Moore neighbours of a coordinate on an unbounded
-- @(Int, Int)@ grid. A different shape of neighbourhood than
-- 'neighborValues': there's no finite domain to search on an infinite
-- grid, so the neighbourhood is computed directly from the
-- coordinates instead of filtered from a universe list -- the same
-- role for a Life-like grid automaton that 'neighborValues' plays for
-- a finite, geometrically-adjacent domain.
moore8 :: (Int, Int) -> [(Int, Int)]
moore8 (x, y) =
  [ (x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0) ]
