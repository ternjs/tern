var util = require("./util");
var tern = require("../lib/tern"), condense = require("../lib/condense");
var fs = require("fs");
require("../plugin/angular");
require("../plugin/node");

function jsonFile(name) { return "test/condense/" + name.replace(/\.js$/, ".json"); }

function runTest(options) {
  var server = new tern.Server({
    defs: [util.ecma5, util.browser],
    plugins: options.plugins
  });
  options.load.forEach(function(file) {
    server.addFile(file, fs.readFileSync("test/condense/" + file, "utf8"));
  });
  server.flush(function() {
    var origins = options.include || options.load;
    var condensed = condense.condense(origins, null, {sortOutput: true});
    var out = JSON.stringify(condensed, null, 2);
    var expect = fs.readFileSync(jsonFile(origins[0]), "utf8").trim();
    if (out != expect)
      return util.failure("condense/" + origins[0] + ": Mismatch in condense output. Got " +
                          out + "\nExpected " + expect);

    // Test loading the condensed defs.
    var server2 = new tern.Server({
      defs: [util.ecma5, util.browser, condensed],
      plugins: options.plugins
    });
    server2.flush(function() {
      var condensed = condense.condense(origins, null, {sortOutput: true});
      var out = JSON.stringify(condensed, null, 2);
      if (out != expect)
        util.failure("condense/" + origins[0] + ": Mismatch in condense output after loading defs. Got " +
                     out + "\nExpected " + expect);
    });
  });
}

exports.runTests = function(filter) {
  function test(options) {
    if (typeof options == "string") options = {load: [options]};
    if (filter && (options.include || options.load)[0].indexOf(filter) == -1) return;
    util.addTest();
    util.addFile();
    runTest(options);
  }

  test("basic.js");
  test("fn.js");
  test("add_to_old.js");
  test({load: ["ignore_newer.js", "extend_foo.js"],
        include: ["ignore_newer.js"]});
  test("ref_to_old.js");
  test("ref_in_type.js");
  test("double_ref.js");
  test("proto.js");
  test("generic.js");

  test({load: ["node_simple.js"], plugins: {node: true}});
  test({load: ["node_fn_export.js"], plugins: {node: true}});

  test({load: ["angular_simple.js"], plugins: {angular: true}});

  test({load: ["requirejs_const.js"], plugins: {requirejs: true}});
  test({load: ["requirejs_primitive.js"], plugins: {requirejs: true}});
  test({load: ["requirejs_setup.js"], plugins: {requirejs: true}});
  test({load: ["requirejs_empty_deps.js"], plugins: {requirejs: true}});
  // TODO(sqs): if load order is reversed, then
  // !define.!requirejs.requirejs_dep.a duplicates the definition instead of
  // referring to !requirejs.requirejs_const.
  test({load: ["requirejs_const.js", "requirejs_dep.js"], include: ["requirejs_dep.js", "requirejs_const.js"], plugins: {requirejs: true}});
};
