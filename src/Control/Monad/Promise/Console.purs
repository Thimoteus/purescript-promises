module Control.Monad.Promise.Console
  ( module C
  , log
  , logShow
  , warn
  , warnShow
  , error
  , errorShow
  , info
  , infoShow
  ) where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE) as C
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Promise (Promise, class Deferred)

log :: forall r. Deferred => String -> Promise ( console :: CONSOLE | r ) Unit
log = liftEff <<< Console.log

logShow :: forall r a. Deferred => Show a => a -> Promise ( console :: CONSOLE | r ) Unit
logShow = liftEff <<< Console.logShow

warn :: forall r. Deferred => String -> Promise ( console :: CONSOLE | r ) Unit
warn = liftEff <<< Console.warn

warnShow :: forall r. Deferred => String -> Promise ( console :: CONSOLE | r ) Unit
warnShow = liftEff <<< Console.warnShow

error :: forall r. Deferred => String -> Promise ( console :: CONSOLE | r ) Unit
error = liftEff <<< Console.error

errorShow :: forall r. Deferred => String -> Promise ( console :: CONSOLE | r ) Unit
errorShow = liftEff <<< Console.errorShow

info :: forall r. Deferred => String -> Promise ( console :: CONSOLE | r ) Unit
info = liftEff <<< Console.info

infoShow :: forall r. Deferred => String -> Promise ( console :: CONSOLE | r ) Unit
infoShow = liftEff <<< Console.infoShow
