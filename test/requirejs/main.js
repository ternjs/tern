// plugin=requirejs

requirejs(["foo", "bar", "useexports", "simplifiedcommon"], function(foo, bar, useexports, simplified) {
  foo.aString; //: string
  bar.aNumber; //: number
  bar.baz.bazProp; //: Date
  bar.baz.bazFooProp; //: string
  useexports.hello; //: bool
  simplified.hello; //: string
  simplified.func; //: fn() -> bool
});
