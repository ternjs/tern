require("../plugin/angular");
require("../plugin/node");
var condenseUtils = require("./condense-utils");

exports.runTests = function(filter) {
  function test(options) { condenseUtils.testConf(condenseUtils.condenseConf, filter, options) };

  test("basic");
  test("fn");
  test("add_to_old");
  test({load: ["ignore_newer", "extend_foo"],
        include: ["ignore_newer"]});
  test("ref_to_old");
  test("ref_in_type");
  test("double_ref");
  test("proto");
  test("generic");
  test("array");
  test("function_prop");

  test({load: ["node_simple"], plugins: {node: true}});
  test({load: ["node_require_private"], plugins: {node: true}});
  test({load: ["node_fn_export"], plugins: {node: true}});
  test({load: ["node_other_module_type_ref"], include: ["node_other_module_type_ref", "node_export_function_a"], plugins: {node: true}});

  test({load: ["angular_simple"], plugins: {angular: true}});

  test({load: ["requirejs_const"], plugins: {requirejs: true}});
  test({load: ["requirejs_primitive"], plugins: {requirejs: true}});
  test({load: ["requirejs_setup"], plugins: {requirejs: true}});
  test({load: ["requirejs_empty_deps"], plugins: {requirejs: true}});
  // TODO(sqs): if load order is reversed, then
  // !define.!requirejs.requirejs_dep.a duplicates the definition instead of
  // referring to !requirejs.requirejs_const.
  test({load: ["requirejs_const", "requirejs_dep"], include: ["requirejs_dep", "requirejs_const"], plugins: {requirejs: true}});

  test("recursive");
}
