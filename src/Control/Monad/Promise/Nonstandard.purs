module Control.Monad.Promise.Nonstandard
  ( done
  , finally
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Promise (Promise)
import Control.Monad.Promise.Unsafe (class Deferred, undefer)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)

foreign import doneImpl
  :: forall r a b c
   . Fn3
   (a -> Eff r c)
   (b -> Eff r c)
   (Promise r a)
   (Eff r Unit)

done :: forall r a c. (a -> Eff r c) -> (Error -> Eff r c) -> (Deferred => Promise r a) -> Eff r Unit
done onSucc onErr p = runFn3 doneImpl onSucc onErr (undefer p)

foreign import finallyImpl
  :: forall r a. Fn2 (Promise r a) (Eff r Unit) (Promise r a)

finally :: forall r a. Deferred => Promise r a -> Eff r Unit -> Promise r a
finally = runFn2 finallyImpl
