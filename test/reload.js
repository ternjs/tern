var tern = require("../lib/tern");
var util = require("./util");

var tests = [], added = false;
function test(name, f) {
  tests.push(function(filter) {
    if (filter && name.indexOf(filter) == -1) return;
    if (!added) { util.addFile(); added = true; }
    util.addTest();
    
    f(new tern.Server({defs: [util.ecma5]}));
  });
}

exports.runTests = function(filter) {
  tests.forEach(function(test) { test(filter); });
};

test("reloadCall", function(server) {
  server.addFile("call.js", "myfun('2');");
  server.addFile("fun.js", "function myfun(x) {\n  x;\n}");
  server.flush(function() {
    var query = {files: [{name: "fun.js", type: "full", text: "function myfun(xy) {\n  xy;\n}"}],
                 query: {type: "type", end: {line: 1, ch: 4}, file: "fun.js"}};
    server.request(query, function(err, data) {
      if (err) return util.failure(err);
      if (data.type != "string") util.failure("reloadCall: Reloading function cleared argument type");
    });
  });
});

test("reloadProps", function(server) {
  server.addFile("obj.js", "var o = {};\no.id = 1234;\no.name = 'test';");
  server.flush(function() {
    var query = {files: [{name: "obj.js", type: "full", text: "var o = {};\no.kapow = 1234;\no.name = 'test';\no."}],
                 query: {type: "completions", end: {line: 3, ch: 2}, file: "obj.js"}};
    server.request(query, function(err, data) {
      if (err) return util.failure(err);
      var compls = data.completions;
      compls.sort();
      if (compls.join() != "kapow,name") util.failure("reloadProps: Reloading properties failed (" + compls + ")");
    });
  });
});
