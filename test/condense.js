var util = require("./util");
var tern = require("../lib/tern"), condense = require("../lib/condense");
var fs = require("fs");
require("../plugin/angular");
require("../plugin/node");

function caseFile(name, ext) { return "test/condense/" + name + "." + (ext || "js"); }

function runTest(options) {
  var server = new tern.Server({
    defs: [util.ecma5, util.browser],
    plugins: options.plugins
  });
  options.load.forEach(function(file) {
    server.addFile(file, fs.readFileSync(caseFile(file), "utf8"));
  });    
  server.flush(function() {
    var condensed = condense.condense(options.include || options.load, null, {sortOutput: true});
    var out = JSON.stringify(condensed, null, 2).trim();
    var expect = fs.readFileSync(caseFile(options.load[0], "json"), "utf8").trim();
    if (out != expect)
      util.failure("condense/" + options.load[0] + ": Mismatch in condense output. Got " +
                   out + "\nExpected " + expect);
  });
}

exports.runTests = function(filter) {
  function test(options) {
    if (typeof options == "string") options = {load: [options]};
    if (filter && options.load[0].indexOf(filter) == -1) return;
    util.addTest();
    util.addFile();
    runTest(options);
  }

  test("basic");
  test("add_to_old");
  test({load: ["ignore_newer", "extend_foo"],
        include: ["ignore_newer"]});
  test("ref_to_old");
  test("ref_in_type");
  test("double_ref");
  test("proto");
  test("generic");

  test({load: ["node_simple"], plugins: {node: true}});
  test({load: ["node_fn_export"], plugins: {node: true}});

  test({load: ["angular_simple"], plugins: {angular: true}});
};
