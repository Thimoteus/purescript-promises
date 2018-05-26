module Effect.Promise.Nonstandard
  ( done
  , finally
  ) where

import Prelude

import Effect (Effect)
import Effect.Exception (Error)
import Effect.Promise (Promise)
import Effect.Promise.Unsafe (class Deferred, undefer)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)

foreign import doneImpl
  :: forall a b c
   . Fn3
   (a -> Effect c)
   (b -> Effect c)
   (Promise a)
   (Effect Unit)

-- | Call's a promise's `done` method, causing execution.
done :: forall a c. (a -> Effect c) -> (Error -> Effect c) -> (Deferred => Promise a) -> Effect Unit
done onSucc onErr p = runFn3 doneImpl onSucc onErr (undefer p)

foreign import finallyImpl
  :: forall a. Fn2 (Promise a) (Effect Unit) (Promise a)

-- | Run the given `Eff` once the given promise settles.
finally :: forall a. Deferred => Promise a -> Effect Unit -> Promise a
finally = runFn2 finallyImpl
