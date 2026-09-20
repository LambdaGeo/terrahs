-- | Bridges 'Control.Comonad.Store.Store' and TerraHS's own
-- 'Coverage' -- the two directions any comonadic spatial model built
-- on 'TerraHS.CA' needs to talk to the rest of TerraHS with.
--
-- A 'Coverage e b' is already "a domain, and a function from the
-- domain to values" -- precisely what 'store' wants, so 'storeAt' is
-- immediate. Going the other way ('storeToCoverage') needs an
-- explicit domain to enumerate, since a 'Store' alone doesn't know
-- which positions are "in bounds".
--
-- Fully generic (no dependency on any particular model's domain
-- type), and shared here, under @examples/common@, by every example
-- built on 'TerraHS.CA' that also wants to render or otherwise use
-- its state as a 'Coverage' (@diffusion-demo@, @fire-demo@). It stays
-- out of @TerraHS.CA@ itself so that library keeps no dependency on
-- the core @terrahs@ library's 'Coverage' type -- see
-- @TerraHS.CA@'s own module haddock.
module StoreBridge
  ( storeAt
  , storeToCoverage
  ) where

import Control.Comonad.Store (Store, store, peek)
import TerraHS (Coverage, covFun, newCov)

-- | A coverage's function, lifted into a 'Store' positioned at a
-- particular domain element.
storeAt :: Coverage e b -> e -> Store e b
storeAt cov = store (covFun cov)

-- | The reverse direction: given the domain to enumerate, turn a
-- 'Store' back into a 'Coverage' over it.
storeToCoverage :: [e] -> Store e b -> Coverage e b
storeToCoverage dom w = newCov dom (`peek` w)
