var bar

var foo = {
  [bar = 100]: 44,
  __proto__: baz,
  a: true,
  method() { return "ok" }
}

var baz = {
  __proto__: null,
  b: "hi"
}

foo.a //: bool
foo.method() //: string
foo.b //: string
foo.toString //: ?
