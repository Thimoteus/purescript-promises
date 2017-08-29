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

**Note**: While promises are [eager](https://medium.com/@avaq/broken-promises-2ae92780f33),
this library provides the `Deferred` typeclass to ensure promises don't prematurely
run their side-effects until safely consumed with `runPromise`, or the nonstandard
`done`.

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
