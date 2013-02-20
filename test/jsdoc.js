/** @type Date */
// a(0): Date
var a = getSomething();

// b: fn() -> number
var b = a.getTime;

/** @type {x: Integer, y: [String]} */
// c: {x: number, y: [string]}
var c = somethingElse();

/**
 * This is a function
 * @return [Number]
 * @param Number a
 * @param String b
 */
// foo: fn(a: number, b: string) -> [number]
function foo(a, b) { return hohoho(); }

/**
 * This is also a function
 * @return string
 * @param Number a
 */
// bar: fn(a: number, b: number) -> string
var bar = function(a, b) { return goop(); };
bar(gulp(), 10);
