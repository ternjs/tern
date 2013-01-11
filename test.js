var fs = require("fs");
var analyze = require("./analyze");

if (process.argv[2]) outputInfo(process.argv[2]);
else runTests();

function runTests() {
  fs.readdirSync("test/").forEach(function(file) {
    var info = analyze.analyze("test/" + file);
    var assertion = /\/\/ (\w+): (.*)\n/g, m;

    while (m = assertion.exec(info.text)) {
      var v = info.env.vars[m[1]];
      if (!v) {
        console.log(file + ": variable " + m[1] + " not defined");
        continue;
      }
      var type = v.aval.toString();
      if (type != m[2])
        console.log(file + ": variable " + m[1] + " has type\n  " + type + "\ninstead of expected type\n  " + m[2]);
    }
  });
}

function outputInfo(file) {
  var info = analyze.analyze(file);
  for (var v in info.env.vars)
    console.log(v + ": " + info.env.vars[v].aval.toString());
}
