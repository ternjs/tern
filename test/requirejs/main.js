// environment=requireJS

requirejs(["foo", "bar"], function(foo, bar) {
  foo.aString; //: string
  bar.aNumber; //: number
  bar.baz.bazProp; //: Date
  bar.baz.bazFooProp; //: string
});
