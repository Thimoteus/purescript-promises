module Data.Promise.Console
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
import Data.Promise (Promise)

log :: forall r. String -> Promise ( console :: CONSOLE | r ) Unit
log = liftEff <<< Console.log

logShow :: forall r a. Show a => a -> Promise ( console :: CONSOLE | r ) Unit
logShow = liftEff <<< Console.logShow

warn :: forall r. String -> Promise ( console :: CONSOLE | r ) Unit
warn = liftEff <<< Console.warn

warnShow :: forall r. String -> Promise ( console :: CONSOLE | r ) Unit
warnShow = liftEff <<< Console.warnShow

error :: forall r. String -> Promise ( console :: CONSOLE | r ) Unit
error = liftEff <<< Console.error

errorShow :: forall r. String -> Promise ( console :: CONSOLE | r ) Unit
errorShow = liftEff <<< Console.errorShow

info :: forall r. String -> Promise ( console :: CONSOLE | r ) Unit
info = liftEff <<< Console.info

infoShow :: forall r. String -> Promise ( console :: CONSOLE | r ) Unit
infoShow = liftEff <<< Console.infoShow
