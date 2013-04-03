var fs = require("fs"), path = require("path");
var infer = require("./infer");
var tern = require("./tern");
var acorn = require("acorn");
var walk = require("acorn/util/walk.js");
require("./plugin/requirejs.js");
require("./plugin/node.js");

var ecma5 = JSON.parse(fs.readFileSync("defs/ecma5.json"));
var defData = {
  browser: JSON.parse(fs.readFileSync("defs/browser.json")),
  jquery: JSON.parse(fs.readFileSync("defs/jquery.json"))
};

function getDefs(text) {
  var spec = /\/\/ environment=(\w+)\n/g, m, defs = [ecma5];
  while (m = spec.exec(text)) {
    var data = defData[m[1]];
    if (!data) throw new Error("Unknown environment: " + m[1]);
    defs.push(data);
  }
  return defs;
}

function getPlugins(text) {
  var spec = /\/\/ plugin=(\w+)\n/g, m, plugins = {};
  while (m = spec.exec(text))
    plugins[m[1]] = m[1] == "node" ? {modules: nodeModules} : {};
  return plugins;
}

var nodeModules = {};
fs.readdirSync("test/node_modules").forEach(function(name) {
  if (/\.json$/.test(name))
    nodeModules[path.basename(name, ".json")] = JSON.parse(fs.readFileSync("test/node_modules/" + name, "utf8"));
});

function serverOptions(context, text) {
  return {
    defs: getDefs(text),
    getFile: function(name) { return fs.readFileSync(path.resolve(context, name), "utf8"); },
    debug: true,
    pluginOptions: { node: { modules: nodeModules } },
    projectDir: context,
    plugins: getPlugins(text)
  };
}

function runTests(filter) {
  var files = 0, tests = 0, failed = 0;
  fs.readdirSync("test/").forEach(function(name) {
    if (filter && name.indexOf(filter) == -1) return;

    ++files;
    var fname = name, context = "test/";
    if (fs.statSync(context + name).isDirectory()) {
      if (name == "node_modules") return;
      context += name + "/";
      fname = "main.js";
    }
    var text = fs.readFileSync(context + fname, "utf8");
    var server = new tern.Server(serverOptions(context, text));
    server.addFile(fname);
    var ast = server.files[0].ast;

    var typedef = /\/\/:(:)?(\?)?\s+([^\n]*)/g, m;
    while (m = typedef.exec(text)) {
      ++tests;
      var expr = walk.findNodeBefore(ast, m.index, "Expression");
      if (!expr) {
        console.log(name + ": No expression found at line " + acorn.getLineInfo(text, m.index).line);
        ++failed;
        continue;
      }
      var query = {type: "type",
                   start: expr.node.start, end: expr.node.end,
                   file: fname,
                   depth: m[1] ? 2 : null};
      server.request({query: query}, function(err, resp) {
        if (err) throw err;
        var type = resp.guess && !m[2] ? "?" : resp.type || "?";
        if (type != m[3]) {
          console.log(name + ": Expression at line " + acorn.getLineInfo(text, m.index).line +
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
