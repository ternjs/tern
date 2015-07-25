function Class() {}
Class.prototype.hello = "string";

var c = new Class;
c.hello = nobodyKnows();

c.hello; //: string
