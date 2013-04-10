var foo = {
  mth: function() {
    this; //: foo
  }
};

function Bar() {}
Bar.prototype.hallo = function() {
  this; //: Bar
};
