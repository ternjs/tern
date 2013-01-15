var fs = require("fs");
var infer = require("./infer");

if (process.argv[2]) outputInfo(process.argv[2]);
else runTests();

function runTests() {
  var files = 0, tests = 0, failed = 0;
  fs.readdirSync("test/").forEach(function(file) {
    ++files;
    infer.withContext(null, function() {
      var info = infer.analyze("test/" + file);
      var assertion = /\/\/ (\w+)(?:\((d+)\))?: (.*)\n/g, m;

      while (m = assertion.exec(info.text)) {
        ++tests;
        var v = info.scope.props[m[1]];
        if (!v) {
          console.log(file + ": variable " + m[1] + " not defined");
          ++failed;
          continue;
        }
        var type = v.toString(Number(m[2] || 5));
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
  infer.withContext(null, function() {
    var info = infer.analyze(file);
    for (var v in info.scope.props)
      console.log(v + ": " + info.scope.props[v].toString(2));
  });
}
