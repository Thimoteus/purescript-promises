# purescript-promises

An alternative effect monad for PureScript.

Use this for easy interop with existing promise-based JavaScript libraries.

## Usage

With monadic `do` notation, or with promise-chaining notation. The following are
equivalent:

```purescript
myApply :: forall r a b. Promise r (a -> b) -> Promise r a -> Promise r b
myApply pab pa = do
  ab <- pab
  a <- pa
  pure (ab a)

myApplyChained :: forall r a b. Promise r (a -> b) -> Promise r a -> Promise r b
myApplyChained pab pa = pab # then' \ ab -> pa # then' \ a -> resolve (ab a)
```

in fact, if you squint a little, `myApplyChained` looks like the following JavaScript:

```javascript
var myApplyChained = function (pab, pa) {
  pab.then(function (ab) {
    pa.then(function (a) {
      return Promise.resolve(ab(a));
    });
  });
}
```

Also see the `tests` folder.

### eagerness

While promises are [eager](https://medium.com/@avaq/broken-promises-2ae92780f33),
this library provides the `Deferred` typeclass to ensure promises don't prematurely
run their side-effects until safely consumed with `runPromise`, or the nonstandard
`done`.

In fact, not only are promises eager, but they're eager about being eager. They *really*
want to run:

### delay example

```purescript
promDelay :: Deferred => Promise AppEff Unit
promDelay = do
  p1
  p2
  p3
  where
    p1 = do
      Console.log "one"
      Promise.delay (Milliseconds 1000.0) unit
      Console.log "two"
    p2 = do
      Promise.delay (Milliseconds 1000.0) unit
      Console.log "three"
    p3 = do
      Promise.delay (Milliseconds 1000.0) unit
      Console.log "four"
```

this will output `one`, wait one second, then `four` `three` `two`.
In order to obtain the desired behavior of waiting one second between
*each* log, it's necessary to add type annotations to `p1`, `p2` and
`p3`:

```purescript
p1 :: Deferred => Promise AppEff Unit
p1 = do ...
```

### parallel `(<*>)`

```purescript
promApply :: Deferred => Promise AppEff Unit
promApply = p1 *> p2 *> p3
  where
    p1 :: Deferred => Promise AppEff Unit
    p1 = do
      Console.log "<*> is"
      Promise.delay (Milliseconds 1000.0) unit
      Console.log "done"
    p2 :: Deferred => Promise AppEff Unit
    p2 = do
      Promise.delay (Milliseconds 3000.0) unit
      Console.log "parallel"
    p3 :: Deferred => Promise AppEff Unit
    p3 = do
      Promise.delay (Milliseconds 2000.0) unit
      Console.log "in"
```

Note that the **only** difference between this example and the last
is that we're using `(*>)` instead of implicit `(>>=)`s. And even
though we added the `Deferred` constraint, it will still take 3
seconds to run total -- not 6, as in the case of `promDelay`.

### FFI example
```javascript
exports.myPromise = new Promise(function (resolve, reject) {
  resolve(5);
});
```

```purescript
foreign import myPromise :: forall r. Promise r Int

doSomething :: Deferred => Promise (console :: CONSOLE | r) Unit
doSomething = do
  p <- myPromise
  Console.logShow p

main :: forall r. Eff (console :: CONSOLE | r) Unit  
main
  = runPromise
    (const (log "success callback"))
    (const (error "error callback"))
    doSomething
```

## Installation

`bower install --save purescript-promises`

## See also
* [purescript-aff-promise](https://github.com/nwolverson/purescript-aff-promise)
* [purescript-aff](https://github.com/slamdata/purescript-aff)
