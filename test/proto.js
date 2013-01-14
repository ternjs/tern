function Foo(x) {
  this.x = x;
  this.y = [1];
}

Foo.prototype = {
  toString: function() { return "hi"; },
  bar: 13
};

var z = new Foo(true);

// Foo: fn(<bool>)
// z: {bar: <number>, toString: fn() -> <string>, x: <bool>, y: {<i>: <number>}}
