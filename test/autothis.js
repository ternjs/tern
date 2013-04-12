function Bar() {}
Bar.prototype.hallo = function() {
  this; //: Bar
};

Bar.prototype.fn2 = function() {
  this; //: Date
};

Date.prototype.fn2 = Bar.prototype.fn2;
new Date().fn2();
