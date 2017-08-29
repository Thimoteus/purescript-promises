module Control.Monad.Promise.Unsafe where

class Deferred

foreign import undefer :: forall a. (Deferred => a) -> a
