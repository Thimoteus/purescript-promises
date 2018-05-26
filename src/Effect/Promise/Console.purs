module Effect.Promise.Console
  ( log
  , logShow
  , warn
  , warnShow
  , error
  , errorShow
  , info
  , infoShow
  ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Promise (Promise, class Deferred)

log :: Deferred => String -> Promise Unit
log = liftEffect <<< Console.log

logShow :: forall a. Deferred => Show a => a -> Promise Unit
logShow = liftEffect <<< Console.logShow

warn :: Deferred => String -> Promise Unit
warn = liftEffect <<< Console.warn

warnShow :: Deferred => String -> Promise Unit
warnShow = liftEffect <<< Console.warnShow

error :: Deferred => String -> Promise Unit
error = liftEffect <<< Console.error

errorShow :: Deferred => String -> Promise Unit
errorShow = liftEffect <<< Console.errorShow

info :: Deferred => String -> Promise Unit
info = liftEffect <<< Console.info

infoShow :: Deferred => String -> Promise Unit
infoShow = liftEffect <<< Console.infoShow
