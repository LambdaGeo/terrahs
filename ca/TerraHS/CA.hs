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
