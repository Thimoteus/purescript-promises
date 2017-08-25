# purescript-promises

An alternative effect monad for PureScript.

Use this for easy interop with existing promise-based JavaScript libraries.

Work in progress!

## Usage

With monadic `do` notation, or with promise-chaining notation. The following are
equivalent:

```purescript
myApply :: forall r a b. Promise r (a -> b) -> Promise a -> Promise b
myApply pab pa = do
  ab <- pab
  a <- pa
  pure (ab a)

myApplyChained :: forall r a b. Promise r (a -> b) -> Promise a -> Promise b
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

see the `tests` folder.

**Note**: As of this current version you'll need to "thunkify" your promises to
keep them from prematurely running (Eff _ solves this problem by automatically
wrapping every action in a JS function of no arguments):

```purescript
myPromise :: forall r. Unit -> Promise (console :: CONSOLE | r) Unit
myPromise = \ _ -> do
  Console.log "hello"
  delay 1000 unit
  Console.log "goodbye"

main :: forall r. Eff (console :: CONSOLE | r) Unit  
main
  = runPromise
    (const (log "success callback"))
    (const (error "error callback"))
    (myPromise unit)
```

## Installation

`bower install --save purescript-promises`

## See also
[purescript-aff-promise](https://github.com/nwolverson/purescript-aff-promise)
