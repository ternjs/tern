var fs = require("fs");
var infer = require("./infer");
var tern = require("./tern");
var acorn = require("acorn");
var walk = require("acorn/util/walk.js");
require("./plugin/requirejs/requirejs.js");

var ecma5 = JSON.parse(fs.readFileSync("defs/ecma5.json"));
var envData = {
  browser: JSON.parse(fs.readFileSync("defs/browser.json")),
  requireJS: JSON.parse(fs.readFileSync("plugin/requirejs/requirejs.json")),
  jquery: JSON.parse(fs.readFileSync("defs/jquery.json"))
};

function getFile(file) {
  var text = fs.readFileSync(file, "utf8"), env = [];
  var envSpec = /\/\/ environment=(\w+)\n/g, m;
  while (m = envSpec.exec(text)) env.push(envData[m[1]]);
  return {text: text, name: file, env: env, ast: acorn.parse(text)};
}

function serverOptions(context, env) {
  var environment = [ecma5];
  for (var i = 0; i < env.length; ++i) environment.push(env[i]);
  return {
    environment: environment,
    getFile: function(name, c) {
      c(null, fs.readFileSync(context + name, "utf8"));
    },
    debug: true
  };
}

function runTests(filter) {
  var files = 0, tests = 0, failed = 0;
  fs.readdirSync("test/").forEach(function(name) {
    if (filter && name.indexOf(filter) == -1) return;

    ++files;
    var fname = name, context = "test/";
    if (fs.statSync(context + name).isDirectory()) {
      context += name + "/";
      fname = "main.js";
    }
    var file = getFile(context + fname);

    var server = new tern.Server(serverOptions(context, file.env));

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
                   file: fname,
                   depth: m[1] ? 2 : null};
      server.request({query: query}, function(err, resp) {
        if (err) throw new Error(err);
        var type = resp.guess && !m[2] ? "?" : resp.type || "?";
        if (type != m[3]) {
          console.log(name + ": Expression at line " + acorn.getLineInfo(file.text, m.index).line +
                      " has type\n  " + type + "\ninstead of expected type\n  " + m[3]);
          ++failed;
        }
      });
    }
  });
  console.log("Ran " + tests + " tests from " + files + " files.");
  console.log(failed ? failed + " failures!" : "All passed.");
}

runTests(process.argv[2]);
