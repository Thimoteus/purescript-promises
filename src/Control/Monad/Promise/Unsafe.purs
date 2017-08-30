module Control.Monad.Promise.Unsafe
  ( class Deferred
  , undefer
  ) where

-- | A class for side-effecting promises which don't prematurely execute.
-- Internal NOTE: this class should always appear as a constraint when an Eff is
-- in negative position and a Promise is in positive position.
class Deferred

-- | Note: use of this function may result in arbitrary side effects.
foreign import undefer :: forall a. (Deferred => a) -> a
