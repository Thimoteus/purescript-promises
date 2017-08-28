module Control.Monad.Promise
  ( Promise
  , PurePromise
  , then'
  , resolve
  , catch
  , reject
  , race
  , all
  , delay
  , runPromise
  ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Monoid (class Monoid, mempty)
import Data.Unfoldable (class Unfoldable)

foreign import data Promise :: # Effect -> Type -> Type

type PurePromise a = forall r. Promise r a

foreign import thenImpl
  :: forall r a b c. Fn3
    (Promise r a)
    (a -> Promise r b)
    (c -> Promise r b)
    (Promise r b)

thenn
  :: forall r a b c. (a -> Promise r b)
  -> (c -> Promise r b)
  -> Promise r a
  -> Promise r b
thenn succ err p =
  let then'' = runFn3 thenImpl
   in then'' p succ err

then' :: forall r a b. (a -> Promise r b) -> Promise r a -> Promise r b
then' = flip thenn reject

foreign import resolve
  :: forall r a. a -> Promise r a

foreign import catchImpl
  :: forall r a b c. Fn2
  (Promise r a)
  (c -> Promise r b)
  (Promise r b)

catchAnything
  :: forall r a c. Promise r a
  -> (c -> Promise r a)
  -> Promise r a
catchAnything = runFn2 catchImpl

catch :: forall r a. Promise r a -> (Error -> Promise r a) -> Promise r a
catch = catchAnything

foreign import rejectImpl :: forall r b c. c -> Promise r b

reject :: forall r b. Error -> Promise r b
reject = rejectImpl

foreign import allImpl :: forall r a. Array (Promise r a) -> Promise r (Array a)

all :: forall f r a. Foldable f => Unfoldable f => f (Promise r a) -> Promise r (f a)
all = map Array.toUnfoldable <<< allImpl <<< Array.fromFoldable

foreign import raceImpl :: forall r a. Array (Promise r a) -> Promise r a

race :: forall f r a. Foldable f => f (Promise r a) -> Promise r a
race = raceImpl <<< Array.fromFoldable

foreign import delayImpl
  :: forall r a. Fn2
  a
  Int
  (Promise r a)

delay :: forall r a. Int -> a -> Promise r a
delay = flip (runFn2 delayImpl)

foreign import promiseToEffImpl
  :: forall eff a b c. Fn3
  (Promise eff a)
  (a -> Eff eff b)
  (c -> Eff eff b)
  (Eff eff Unit)

runPromise
  :: forall eff a b. (a -> Eff eff b)
  -> (Error -> Eff eff b)
  -> (Unit -> Promise eff a)
  -> Eff eff Unit
runPromise onSucc onErr p = runFn3 promiseToEffImpl (p unit) onSucc onErr

instance functorPromise :: Functor (Promise r) where
  map :: forall r a b. (a -> b) -> Promise r a -> Promise r b
  map f promise = promise # then' \ a -> resolve (f a)

instance applyPromise :: Apply (Promise r) where
  apply :: forall r a b. Promise r (a -> b) -> Promise r a -> Promise r b
  apply pf pa =
    pf # then' \ f -> pa # then' \ a -> resolve (f a)

instance applicativePromise :: Applicative (Promise r) where
  pure = resolve

instance bindPromise :: Bind (Promise r) where
  bind :: forall r a b. Promise r a -> (a -> Promise r b) -> Promise r b
  bind = flip then'

instance monadPromise :: Monad (Promise r)

instance monadThrowPromise :: MonadThrow Error (Promise r) where
  throwError :: forall r a. Error -> Promise r a
  throwError = reject

instance monadErrorPromise :: MonadError Error (Promise r) where
  catchError :: forall r a. Promise r a -> (Error -> Promise r a) -> Promise r a
  catchError = catch

instance semigroupPromise :: Semigroup a => Semigroup (Promise r a) where
  append :: forall r a. Semigroup a => Promise r a -> Promise r a -> Promise r a
  append a b = append <$> a <*> b

instance monoidPromise :: Monoid a => Monoid (Promise r a) where
  mempty :: forall r a. Monoid a => Promise r a
  mempty = resolve mempty

instance monadEffPromise :: MonadEff r (Promise r) where
  liftEff :: forall eff a. Eff eff a -> Promise eff a
  liftEff eff = resolve (unsafePerformEff eff)
