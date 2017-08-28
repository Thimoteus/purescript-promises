exports.doneImpl = function (onSuccess, onError, promise) {
  return function () {
    // polyfill taken from promisejs.org
    if (typeof Promise.prototype.done !== 'function') {
      Promise.prototype.done = function (onFulfilled, onRejected) {
        var self = arguments.length ? this.then.apply(this, arguments) : this
        self.then(null, function (err) {
          setTimeout(function () {
            throw err
          }, 0)
        });
      }
    }
    promise.done(onSuccess, onError);
    return null;
  }
}

exports.finallyImpl = function (promise, eff) {
  // polyfill taken from promisejs.org
  if (typeof Promise.prototype['finally'] !== 'function') {
    Promise.prototype['finally'] = function (f) {
      return this.then(function (value) {
        return Promise.resolve(f()).then(function () {
          return value;
        });
      }, function (err) {
        return Promise.resolve(f()).then(function () {
          throw err;
        });
      });
    }
  }
  return promise.finally(eff);
}
