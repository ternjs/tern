function hello(a, b) {
  var c = a + b;
  hello(a, c);
  a; //refs: 1,15 2,10 3,8, 4,2
  c; //refs: 2,6 3,11 5,2
}

hello; //refs: 1,9 3,2 8,0

var obj = {
  x //<refs: 11,2 16,4 19,4
  : 10,
  y: 20
};

obj.x = 30;
obj.z = "hi";

obj.x;
obj.z; //refs: 17,4 20,4

class foo {

  methodA //<refs: 24,2 41,2 44,2
  (){}
  
  methodB //<refs: 27,2 42,2
  () {} 
}

class bar extends foo {

  methodB //<refs: 33,2 45,2
  () {}

}

var a = new foo();
var b = new bar();

a.methodA; //refs: 24,2 41,2 44,2
a.methodB; //refs: 27,2 42,2

b.methodA; //refs: 24,2 41,2 44,2
b.methodB; //refs: 33,2 45,2
