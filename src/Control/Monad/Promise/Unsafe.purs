module Control.Monad.Promise.Unsafe where

-- | A class for side-effecting promises which don't prematurely execute.
class Deferred

-- | Note: use of this function may result in arbitrary side effects.
foreign import undefer :: forall a. (Deferred => a) -> a
