exports.thenImpl = function (promise, onFulfilled, onRejected) {
  return promise.then(onFulfilled, onRejected);
};

exports.catchImpl = function (promise, f) {
  return promise.catch(f);
}

exports.resolve = function (a) {
  return Promise.resolve(a);
}

exports.rejectImpl = function (a) {
  return Promise.reject(a);
}

exports.promiseToEffImpl = function (promise, onFulfilled, onRejected) {
  return function () {
    promise.then(onFulfilled(), onRejected());
    return null;
  }
}

exports.delayImpl = function (a, ms) {
  return new Promise(function (resolve, reject) {
    setTimeout(resolve, ms, a);
  });
}
