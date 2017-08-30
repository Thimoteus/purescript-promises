module Control.Monad.Promise
  ( Promise
  , PurePromise
  , promise
  , then_
  , then'
  , resolve
  , catch
  , reject
  , race
  , all
  , delay
  , runPromise
  , module Exports
  ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION, Error, throwException)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Promise.Unsafe (class Deferred) as Exports
import Control.Monad.Promise.Unsafe (class Deferred, undefer)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn2, runFn3)
import Data.Monoid (class Monoid, mempty)
import Data.Time.Duration (Milliseconds)
import Data.Unfoldable (class Unfoldable)

-- | A JavaScript promise parameterized (in the type) by a row of effects.
foreign import data Promise :: # Effect -> Type -> Type

-- | A promise with no side-effects.
type PurePromise a = forall r. Promise r a

foreign import promiseImpl :: forall r a b.
  (Fn2 (a -> Eff r Unit) (b -> Eff r Unit) (Eff r Unit)) -> Promise r a

-- | Create a promise by supplying a function which takes a success callback and
-- | error callback as arguments, returning an `Eff`.
promise :: forall r a. Deferred => ((a -> Eff r Unit) -> (Error -> Eff r Unit) -> Eff r Unit) -> Promise r a
promise k = promiseImpl (mkFn2 k)

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
thenn succ err p = runFn3 thenImpl p succ err

-- | Given a promise and a function which uses that promise's resolved value,
-- | create a new promise that resolves to the function's output.
then' :: forall r a b. Deferred => (a -> Promise r b) -> Promise r a -> Promise r b
then' = flip thenn reject

-- | Useful for when you need to transform an error and a resolved value into
-- | the same type.
then_
  :: forall r a b. Deferred
  => (a -> Promise r b)
  -> (Error -> Promise r b)
  -> Promise r a
  -> Promise r b
then_ = thenn

foreign import resolveImpl
  :: forall r a. a -> Promise r a

-- | Create a promise from a value.
resolve :: forall r a. a -> Promise r a
resolve = resolveImpl

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

-- | Deals with any errors that may be thrown by the given promise.
catch :: forall r a. Deferred => Promise r a -> (Error -> Promise r a) -> Promise r a
catch = catchAnything

foreign import rejectImpl :: forall r b c. c -> Promise r b

-- | Throw an error into a promise.
reject :: forall r b. Deferred => Error -> Promise r b
reject = rejectImpl

attempt :: forall r a. Deferred => Promise r a -> Promise r (Either Error a)
attempt p = p # then_ (resolve <<< Right) (resolve <<< Left)

foreign import allImpl :: forall r a. Array (Promise r a) -> Promise r (Array a)

-- | Run all promises in the given `Foldable`, returning a new promise which either
-- | resolves to a collection of all the given promises' results, or rejects with
-- | the first promise to reject.
all :: forall f g r a. Deferred => Foldable f => Unfoldable g => f (Promise r a) -> Promise r (g a)
all = map Array.toUnfoldable <<< allImpl <<< Array.fromFoldable

foreign import raceImpl :: forall r a. Array (Promise r a) -> Promise r a

-- | Note that while promise semantics say that `race xs` resolves to the first
-- | `x` in `xs` to resolve, `race xs` won't terminate until each promise is
-- | settled.
-- | In addition, if `Array.fromFoldable xs` is `[]`, `race xs` will never settle.
race :: forall f r a. Deferred => Foldable f => f (Promise r a) -> Promise r a
race = raceImpl <<< Array.fromFoldable

foreign import delayImpl
  :: forall r a. Fn2
  a
  Milliseconds
  (Promise r a)

-- | Cause a delay in execution, then resolve with the given value.
delay :: forall r a. Deferred => Milliseconds -> a -> Promise r a
delay = flip (runFn2 delayImpl)

foreign import promiseToEffImpl
  :: forall eff a b c. Fn3
  (Promise eff a)
  (a -> Eff eff b)
  (c -> Eff eff b)
  (Eff eff Unit)

-- | Consume a promise. Note that this is the only standard way to safely
-- | discharge the `Deferred` constraints you are likely to have.
runPromise
  :: forall eff a b. (a -> Eff eff b)
  -> (Error -> Eff eff b)
  -> (Deferred => Promise eff a)
  -> Eff eff Unit
runPromise onSucc onErr p = runFn3 promiseToEffImpl (undefer p) onSucc onErr

yoloPromise :: forall eff a. (Deferred => Promise eff a) -> Eff (exception :: EXCEPTION | eff) Unit
yoloPromise dp = addEx $ runPromise (const (pure unit)) (removeEx <<< throwException) dp
  where
    removeEx :: Eff (exception :: EXCEPTION | eff) Unit -> Eff eff Unit
    removeEx = unsafeCoerceEff
    addEx :: Eff eff Unit -> Eff (exception :: EXCEPTION | eff) Unit
    addEx = unsafeCoerceEff

instance functorPromise :: Deferred => Functor (Promise r) where
  map :: forall r a b. Deferred => (a -> b) -> Promise r a -> Promise r b
  map f p = p # then' \ a -> resolve (f a)

instance applyPromise :: Deferred => Apply (Promise r) where
  apply :: forall r a b. Deferred => Promise r (a -> b) -> Promise r a -> Promise r b
  apply pf pa =
    pf # then' \ f -> pa # then' \ a -> resolve (f a)

instance applicativePromise :: Deferred => Applicative (Promise r) where
  pure = resolve

instance bindPromise :: Deferred => Bind (Promise r) where
  bind :: forall r a b. Deferred => Promise r a -> (a -> Promise r b) -> Promise r b
  bind = flip then'

instance monadPromise :: Deferred => Monad (Promise r)

instance monadThrowPromise :: Deferred => MonadThrow Error (Promise r) where
  throwError :: forall r a. Deferred => Error -> Promise r a
  throwError = reject

instance monadErrorPromise :: Deferred => MonadError Error (Promise r) where
  catchError :: forall r a. Deferred => Promise r a -> (Error -> Promise r a) -> Promise r a
  catchError = catch

instance semigroupPromise :: (Deferred, Semigroup a) => Semigroup (Promise r a) where
  append :: forall r a. Deferred => Semigroup a => Promise r a -> Promise r a -> Promise r a
  append a b = append <$> a <*> b

instance monoidPromise :: (Deferred, Monoid a) => Monoid (Promise r a) where
  mempty :: forall r a. Deferred => Monoid a => Promise r a
  mempty = resolve mempty

foreign import liftEffImpl :: forall eff a. Eff eff a -> Promise eff a
instance monadEffPromise :: Deferred => MonadEff r (Promise r) where
  liftEff :: forall eff a. Deferred => Eff eff a -> Promise eff a
  liftEff = liftEffImpl
