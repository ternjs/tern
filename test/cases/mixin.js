function Class() {}
Class.prototype = mixin({
  // M3
  m3: function() {}
})

function mixin(obj) {
  obj.m1 = m1
  obj.m2 = m2
  return obj
}

// M1
function m1() {}
// M2
function m2() {}

function OtherClass() {}
OtherClass.prototype = mixin({
  // M4
  m4: function() {}
})

let c = new Class

c.m1 //doc: M1
c.m2 //doc: M2
c.m3 //doc: M3
c.m4 //: ?

let oc = new OtherClass

oc.m1 //doc: M1
oc.m3 //: ?
oc.m4 //doc: M4

mixin(new Date) //: Date
mixin("foo") //: string
