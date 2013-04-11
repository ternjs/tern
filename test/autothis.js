var foo = {
  mth: function() {
    this; //: foo
  },
  fn2: function() {
    // Verify that actual calls take precedence
    this; //: Bar
  }
};

function Bar() {}
Bar.prototype.hallo = function() {
  this; //: Bar
};
Bar.prototype.fn2 = foo.fn2;

new Bar().fn2();
