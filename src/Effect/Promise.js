exports.thenImpl = function (promise, onFulfilled, onRejected) {
  return promise.then(onFulfilled, onRejected);
};

exports.catchImpl = function (promise, f) {
  return promise.catch(f);
};

exports.resolveImpl = function (a) {
  return Promise.resolve(a);
};

exports.rejectImpl = function (a) {
  return Promise.reject(a);
};

exports.promiseToEffectImpl = function (promise, onFulfilled, onRejected) {
  return function () {
    return promise.then(function (a) {
      return onFulfilled(a)();
    }, function (err) {
      return onRejected(err)();
    });
  };
};

exports.allImpl = function (arr) {
  return Promise.all(arr);
};

exports.raceImpl = function (arr) {
  return Promise.race(arr);
};

exports.liftEffectImpl = function (eff) {
  return new Promise(function (onSucc, onErr) {
    try {
      result = eff();
    } catch (err) {
      return onErr(err);
    }
    return onSucc(result);
  });
};

exports.promiseImpl = function (callback) {
  return new Promise(function(resolve, reject) {
    callback(function (a) {
      return function () {
        resolve(a);
      };
    }, function (err) {
      return function () {
        reject(err);
      };
    })();
  });
};

exports.delayImpl = function (a, ms) {
  return new Promise(function (resolve, reject) {
    setTimeout(resolve, ms, a);
  });
};
