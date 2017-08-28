module Control.Monad.Promise.Nonstandard
  ( done
  , finally
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Promise (Promise)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)

foreign import doneImpl
  :: forall r a b c
   . Fn3
   (a -> Eff r c)
   (b -> Eff r c)
   (Promise r a)
   (Eff r Unit)

done :: forall r a c. (a -> Eff r c) -> (Error -> Eff r c) -> Promise r a -> Eff r Unit
done = runFn3 doneImpl

foreign import finallyImpl
  :: forall r a. Fn2 (Promise r a) (Eff r Unit) (Promise r a)

finally :: forall r a. Promise r a -> Eff r Unit -> Promise r a
finally = runFn2 finallyImpl
