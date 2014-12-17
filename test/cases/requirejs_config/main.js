// plugin=requirejs

requirejs.config({
  paths: {
    fooAlias: "../requirejs/foo"
  }
});

require(["fooAlias"], function(foo) {
  foo.aString; //: string
});

requirejs.config({
  p //+ packages, paths, ...
});

requirejs.config({
  //+ baseUrl, config, context, map, nodeIdCompat, packages, paths, shim, ...
});
