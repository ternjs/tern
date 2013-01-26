// Foo: fn(x: bool)
function Foo(x) {
  this.x = x;
  this.y = [1];
}

Foo.prototype = {
  makeString: function() { return "hi"; },
  bar: 13
};

// z: {x: bool, y: [number]}
var z = new Foo(true);

// z_makeString: fn() -> string
var z_makeString = z.toString;

// z_bar: number
var z_bar = z.bar;
