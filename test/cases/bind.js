function f(a, b, c, d) { return a + b + c + d; }
var g = f.bind(null, 1, 2);

g(2, 3); //: number
g; //: fn(c: number, d: number) -> number

function h(a) { return {a: a, th: this}; }
var i = h.bind({str: "foo"}, 2);

i.call({x: 1}); //:: {a: number, th: {str: string}}

var o = {i: i};
o.i(); //:: {a: number, th: {str: string}}

function j() { return this; }
var k = j.bind({a: true});

k.call({b: false});
k(); //:: {a: bool}
