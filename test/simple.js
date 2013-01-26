// foo: number
var foo = (function() {
  return 42;
})();

// x: {bar: number, foo: number}
var x = {};

// init: fn(v: {bar: number, foo: number})
function init(v) {
  v.foo = 10;
  v.bar = 1 + 1;
}

init(x);
