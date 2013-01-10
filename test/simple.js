var foo = (function() {
  return 42;
})();

var x = {};

function init(v) {
  v.foo = 10;
  v.bar = 1 + 1;
}

init(x);