// Foo: fn(<bool>)
function Foo(x) {
  this.x = x;
  this.y = [1];
}

Foo.prototype = {
  toString: function() { return "hi"; },
  bar: 13
};

// z: {bar: <number>, toString: fn() -> <string>, x: <bool>, y: [<number>]}
var z = new Foo(true);
