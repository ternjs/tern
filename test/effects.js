// a: [number]
var a = ["foo", "bar"].map(function(s) { return s.charCodeAt(0); });

// b: [bool]
var b = [];
b.push(true);

// c: [?]
var c = [];
c.push("hi");
c.push(10);

// d: number
var d;
function setD(a) { d = a; }
setD.call(null, 55);