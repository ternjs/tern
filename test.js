var fs = require("fs");
var infer = require("./infer");

var ecma5 = JSON.parse(fs.readFileSync("ecma5.json"));
var envData = {
  browser: JSON.parse(fs.readFileSync("browser.json"))
};

if (process.argv[2]) outputInfo(process.argv[2]);
else runTests();

function getFile(file) {
  var text = fs.readFileSync(file, "utf8"), env = [ecma5];
  var envSpec = /\/\/ environment=(\w+)\n/g, m;
  while (m = envSpec.exec(text)) env.push(envData[m[1]]);
  return {text: text, file: file, env: env};
}

function runTests() {
  var files = 0, tests = 0, failed = 0;
  fs.readdirSync("test/").forEach(function(file) {
    ++files;
    var data = getFile("test/" + file);
    var cx = new infer.Context(data.env);
    infer.withContext(cx, function() {
      var info = infer.analyze(data.text, data.file);
      var assertion = /\/\/ (\w+)(?:\((\d+)\))?: (.*)\n/g, m;

      while (m = assertion.exec(info.text)) {
        ++tests;
        var v = cx.topScope.findVar(m[1]);
        if (!v) {
          console.log(file + ": variable " + m[1] + " not defined");
          ++failed;
          continue;
        }
        var type = infer.toString(v.getType(), Number(m[2] || 5));
        if (type != m[3]) {
          console.log(file + ": variable " + m[1] + " has type\n  " + type + "\ninstead of expected type\n  " + m[3]);
          ++failed;
        }
      }
    });
  });
  console.log("Ran " + tests + " tests from " + files + " files.");
  console.log(failed ? failed + " failures!" : "All passed.");
}

function outputInfo(file) {
  var data = getFile(file);
  var cx = new infer.Context(data.env);
  infer.withContext(cx, function() {
    infer.analyze(data.text, data.file);
    for (var i = 3; i < process.argv.length; ++i) {
      var v = process.argv[i], found = cx.topScope.findVar(v);
      console.log(v + ": " + infer.toString(found && found.getType(), 2));
    }
  });
}
