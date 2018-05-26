module Test.Main where

import Prelude

import Effect.Console (log)
import Effect.Exception (Error, message, stack)
import Effect.Promise as Promise
import Effect.Promise.Console as Console
import Effect.Promise.Nonstandard as NS
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)

type Promise a = Promise.Deferred => Promise.Promise a

print :: String -> Promise Unit
print msg = do
  Console.log "\n"
  Console.log msg
  Console.log "-------------"

main :: Effect Unit
main = Promise.runPromise onSuccess onError promiseTests

promiseTests :: Promise Unit
promiseTests = do
  print "Basic do notation:"
  doTest

  print "Basic promise delay"
  promDelay

  print "Apply test"
  promApply

  print "Nonstandard methods:"
  NS.finally nonstandardTest (log "'finally' called")

doTest :: Promise Unit
doTest = do
  msg <- pure "Now I'm here"
  Console.log "I'm here"
  Promise.delay (Milliseconds 1000.0) unit
  Console.log msg

noopPrep :: Promise String
noopPrep = Promise.promise k
  where
  k onSucc _ = onSucc "this shouldn't be shown on console"

noop :: Promise Unit
noop = do
  msg <- noopPrep
  Console.log msg

promDelay :: Promise Unit
promDelay = do
  p1
  p2
  p3
  where
    p1 :: Promise Unit
    p1 = do
      Console.log "one"
      Promise.delay (Milliseconds 1000.0) unit
      Console.log "two"
    p2 :: Promise Unit
    p2 = do
      Promise.delay (Milliseconds 1000.0) unit
      Console.log "three"
    p3 :: Promise Unit
    p3 = do
      Promise.delay (Milliseconds 1000.0) unit
      Console.log "four"

promApply :: Promise Unit
promApply = p1 *> p2 *> p3
  where
    p1 :: Promise Unit
    p1 = do
      Console.log "<*> is"
      Promise.delay (Milliseconds 1000.0) unit
      Console.log "done"
    p2 :: Promise Unit
    p2 = do
      Promise.delay (Milliseconds 3000.0) unit
      Console.log "parallel"
    p3 :: Promise Unit
    p3 = do
      Promise.delay (Milliseconds 2000.0) unit
      Console.log "in"

nonstandardTest :: Promise Unit
nonstandardTest = Promise.resolve "nonstandard test" # Promise.then' \ a -> Console.log a

onError :: Error -> Effect Unit
onError e
  = log $ "Uh oh, an error happened! " <>
    message e <>
    "\nStack: " <>
    fromMaybe "No stack." (stack e)

onSuccess :: forall a. a -> Effect Unit
onSuccess _ = log "Hello from Eff!"
