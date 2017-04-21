// environment=jquery

function A() {}

A.fn = A.prototype;

jQuery.extend(A, {
  foo: function() {
    return "str";
  }
});

jQuery.extend(A.fn, {
  foo: function() {
    return 123;
  }
});

A.foo; //: fn() -> string
(new A()).foo; //: fn() -> number
