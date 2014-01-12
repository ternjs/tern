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
