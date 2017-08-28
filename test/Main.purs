module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Promise as Promise
import Control.Monad.Promise.Nonstandard as NS
import Control.Monad.Promise.Console as Console

type AppEff = (console :: CONSOLE)

main :: Eff AppEff Unit
main = do
  NS.done onSuccess onError (NS.finally (prom2 unit) (log "hi"))
  Promise.runPromise onSuccess onError prom2

prom1 :: Unit -> Promise.Promise AppEff Unit
prom1 _ = do
  p1 <- pure "Now I'm here"
  Console.log "I'm here"
  Promise.delay 1000 unit
  Console.log p1

prom2 :: Unit -> Promise.Promise AppEff Unit
prom2 _ = Promise.resolve "hello" # Promise.then' \ a -> Console.log a

onError :: forall r. Error -> Eff (console :: CONSOLE | r) Unit
onError _ = log "oh noes!"

onSuccess :: forall r. Unit -> Eff (console :: CONSOLE | r) Unit
onSuccess _ = log "hello from Eff"
