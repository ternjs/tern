function Ctor1() { this.x = 10; }
Ctor1.prototype = {a: 1};

function Ctor2() {}

var singleton = {a: 10, b: 20};

var o1 = new Ctor1();
var o2 = new Ctor2();

// singleton(0): singleton
// o1(0): Ctor1
// o2(0): Ctor2
