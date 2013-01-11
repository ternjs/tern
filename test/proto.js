function Foo(x) {
  this.x = x;
  this.y = [];
}

Foo.prototype = {
  toString: function() { return "hi"; },
  bar: 13
};

var z = new Foo(true);

// Foo: fn(bool)
// z: {toString: fn() -> string), bar: number, x: bool, y: {<i>: <empty>}}
