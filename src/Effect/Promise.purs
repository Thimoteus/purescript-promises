module Effect.Promise
  ( Promise
  , promise
  , then_
  , then'
  , resolve
  , catch
  , reject
  , race
  , attempt
  , apathize
  , all
  , delay
  , runPromise
  , module Exports
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn2, runFn3)
import Data.Time.Duration (Milliseconds)
import Data.Unfoldable (class Unfoldable)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, throwException)
import Effect.Promise.Unsafe (class Deferred) as Exports
import Effect.Promise.Unsafe (class Deferred, undefer)

-- | A JavaScript promise.
foreign import data Promise :: Type -> Type

foreign import promiseImpl :: forall a b.
  (Fn2 (a -> Effect Unit) (b -> Effect Unit) (Effect Unit)) -> Promise a

-- | Create a promise by supplying a function which takes a success callback and
-- | error callback as arguments, returning an `Effect`.
promise :: forall a. Deferred => ((a -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit) -> Promise a
promise k = promiseImpl (mkFn2 k)

foreign import thenImpl
  :: forall a b c. Fn3
    (Promise a)
    (a -> Promise b)
    (c -> Promise b)
    (Promise b)

generalThen
  :: forall a b c. (a -> Promise b)
  -> (c -> Promise b)
  -> Promise a
  -> Promise b
generalThen succ err p = runFn3 thenImpl p succ err

-- | Given a promise and a function which uses that promise's resolved value,
-- | create a new promise that resolves to the function's output.
then' :: forall a b. Deferred => (a -> Promise b) -> Promise a -> Promise b
then' = flip generalThen reject

-- | Useful for when you need to transform an error and a resolved value into
-- | the same type.
then_
  :: forall a b. Deferred
  => (a -> Promise b)
  -> (Error -> Promise b)
  -> Promise a
  -> Promise b
then_ = generalThen

foreign import resolveImpl
  :: forall a. a -> Promise a

-- | Create a promise from a value.
resolve :: forall a. a -> Promise a
resolve = resolveImpl

foreign import catchImpl
  :: forall a b c. Fn2
  (Promise a)
  (c -> Promise b)
  (Promise b)

catchAnything
  :: forall a c. Promise a
  -> (c -> Promise a)
  -> Promise a
catchAnything = runFn2 catchImpl

-- | Deals with any errors that may be thrown by the given promise.
catch :: forall a. Deferred => Promise a -> (Error -> Promise a) -> Promise a
catch = catchAnything

foreign import rejectImpl :: forall b c. c -> Promise b

-- | Throw an error into a promise.
reject :: forall b. Deferred => Error -> Promise b
reject = rejectImpl

-- | Same as `try`.
attempt :: forall a. Deferred => Promise a -> Promise (Either Error a)
attempt p = p # then_ (\ a -> resolve (Right a)) (\ err -> resolve (Left err))

apathize :: forall a. Deferred => Promise a -> Promise Unit
apathize p = const unit <$> attempt p

foreign import allImpl :: forall a. Array (Promise a) -> Promise (Array a)

-- | Run all promises in the given `Foldable`, returning a new promise which either
-- | resolves to a collection of all the given promises' results, or rejects with
-- | the first promise to reject.
all :: forall f g a. Deferred => Foldable f => Unfoldable g => f (Promise a) -> Promise (g a)
all = map Array.toUnfoldable <<< allImpl <<< Array.fromFoldable

foreign import raceImpl :: forall a. Array (Promise a) -> Promise a

-- | Note that while promise semantics say that `race xs` resolves to the first
-- | `x` in `xs` to resolve, `race xs` won't terminate until each promise is
-- | settled.
-- | In addition, if `Array.fromFoldable xs` is `[]`, `race xs` will never settle.
race :: forall f a. Deferred => Foldable f => f (Promise a) -> Promise a
race = raceImpl <<< Array.fromFoldable

foreign import delayImpl :: forall a. Fn2 a Milliseconds (Promise a)

-- | Cause a delay in execution, then resolve with the given value.
delay :: forall a. Deferred => Milliseconds -> a -> Promise a
delay = flip (runFn2 delayImpl)

foreign import promiseToEffectImpl
  :: forall a b c. Fn3
  (Promise a)
  (a -> Effect b)
  (c -> Effect b)
  (Effect Unit)

-- | Consume a promise. Note that this is the only standard way to safely
-- | discharge the `Deferred` constraints you are likely to have.
runPromise
  :: forall a b. (a -> Effect b)
  -> (Error -> Effect b)
  -> (Deferred => Promise a)
  -> Effect Unit
runPromise onSucc onErr p = runFn3 promiseToEffectImpl (undefer p) onSucc onErr

yoloPromise :: forall a. (Deferred => Promise a) -> Effect Unit
yoloPromise dp = runPromise (const (pure unit)) throwException dp

instance functorPromise :: Deferred => Functor Promise where
  map :: forall a b. Deferred => (a -> b) -> Promise a -> Promise b
  map f p = p # then' \ a -> resolve (f a)

instance applyPromise :: Deferred => Apply Promise where
  apply :: forall a b. Deferred => Promise (a -> b) -> Promise a -> Promise b
  apply pf pa = pf # then' \ f -> pa # then' \ a -> resolve (f a)

instance applicativePromise :: Deferred => Applicative Promise where
  pure = resolve

instance bindPromise :: Deferred => Bind Promise where
  bind :: forall a b. Deferred => Promise a -> (a -> Promise b) -> Promise b
  bind = flip then'

instance monadPromise :: Deferred => Monad Promise

instance monadThrowPromise :: Deferred => MonadThrow Error Promise where
  throwError :: forall a. Deferred => Error -> Promise a
  throwError = reject

instance monadErrorPromise :: Deferred => MonadError Error Promise where
  catchError :: forall a. Deferred => Promise a -> (Error -> Promise a) -> Promise a
  catchError = catch

instance semigroupPromise :: (Deferred, Semigroup a) => Semigroup (Promise a) where
  append :: forall a. Deferred => Semigroup a => Promise a -> Promise a -> Promise a
  append a b = append <$> a <*> b

instance monoidPromise :: (Deferred, Monoid a) => Monoid (Promise a) where
  mempty :: forall a. Deferred => Monoid a => Promise a
  mempty = resolve mempty

foreign import liftEffectImpl :: forall a. Effect a -> Promise a
instance monadEffectPromise :: Deferred => MonadEffect Promise where
  liftEffect :: forall a. Deferred => Effect a -> Promise a
  liftEffect = liftEffectImpl
