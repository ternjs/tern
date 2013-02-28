var fs = require("fs");
var infer = require("./infer");
var tern = require("./tern");
var acorn = require("acorn");
var walk = require("acorn/util/walk.js");

var ecma5 = JSON.parse(fs.readFileSync("ecma5.json"));
var envData = {
  browser: JSON.parse(fs.readFileSync("browser.json"))
};

function getFile(file) {
  var text = fs.readFileSync(file, "utf8"), env = [];
  var envSpec = /\/\/ environment=(\w+)\n/g, m;
  while (m = envSpec.exec(text)) env.push(envData[m[1]]);
  return {text: text, name: file, env: env, ast: acorn.parse(text)};
}

function fetchFile(name, c) {
  return c(null, fs.readFileSync(name, "utf8"));
}

function runTests() {
  var files = 0, tests = 0, failed = 0;
  fs.readdirSync("test/").forEach(function(name) {
    ++files;
    var file = getFile("test/" + name);

    var server = new tern.Server({getFile: fetchFile});
    server.addFile(file.name);
    server.addEnvironment(ecma5);
    for (var i = 0; i < file.env.length; ++i) server.addEnvironment(file.env[i]);

    var typedef = /\/\/:(:)?(\?)?\s+([^\n]*)/g, m;
    while (m = typedef.exec(file.text)) {
      ++tests;
      var expr = walk.findNodeBefore(file.ast, m.index, "Expression");
      if (!expr) {
        console.log(name + ": No expression found at line " + acorn.getLineInfo(file.text, m.index).line);
        ++failed;
        continue;
      }
      var query = {type: "type",
                   start: expr.node.start, end: expr.node.end,
                   file: file.name,
                   depth: m[1] ? 2 : null};
      server.request({query: query}, function(err, resp) {
        if (err) throw new Error(err);
        var type = resp.guess && !m[2] ? "?" : resp.type || "?";
        if (type != m[3]) {
          console.log(name + ": Expression at line " + acorn.getLineInfo(file.text, m.index).line +
                      " has type\n  " + resp.type + "\ninstead of expected type\n  " + m[3]);
          ++failed;
        }
      });
    }
  });
  console.log("Ran " + tests + " tests from " + files + " files.");
  console.log(failed ? failed + " failures!" : "All passed.");
}

runTests();
