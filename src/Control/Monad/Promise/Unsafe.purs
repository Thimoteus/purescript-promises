module Control.Monad.Promise.Unsafe where

class Deferred

-- | Note: use of this function may result in arbitrary side effects.
foreign import undefer :: forall a. (Deferred => a) -> a
