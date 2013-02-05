var base = {foo: 10, bar: 20};
var gen1 = Object.create(base);
var gen2 = Object.create(gen1);

base.baz = 30;
gen1.quux = 50;
gen2.kaka = 10;

// a: number
var a = gen1.foo;

// b: number
var b = gen2.foo;

// c: number
var c = gen1.baz;

// d: number
var d = gen2.baz;

// e: number
var e = gen1.quux;

// f: number
var f = gen2.quux;

// g: ?
var g = gen1.kaka;

var extend = Object.create(base, {prop1: {value: "hi"}, prop2: {}});

// h: string
var h = extend.prop1;

// i: number
var i = extend.bar;

var empty = Object.create(null);

// hop: ?
var hop = empty.hasOwnProperty;
