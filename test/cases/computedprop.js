var x = {};
x[foo()] = {a: 10, b: 20};
x.bar.a; //: number
x.bar. //+? a, b

var obj = {a: "bar", b: "baz"};
obj[foo()]; //:? string
