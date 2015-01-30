var util = require("./util");
var tern = require("../lib/tern"), condense = require("../lib/condense");
var fs = require("fs"), path = require("path");

var condenseDir = "test/condense";
exports.condenseConf = {
  projectDir: path.resolve(__dirname, ".."),
  resolve: function(pth) { return path.resolve(this.projectDir, pth); },
  jsonFile: function(name) { return this.resolve(condenseDir + "/" + name.replace(/\.js$/, ".json")) }
}

exports.runTest = function(conf,options) {
  var server = new tern.Server({
    defs: [util.ecma5, util.browser],
    plugins: options.plugins,
    projectDir: conf.resolve(condenseDir),
    getFile: function(name) {
      return fs.readFileSync(path.resolve(condenseDir, name), "utf8");
    }
  });
  options.load.forEach(function(file) {
    server.addFile(file);
  });
  server.flush(function() {
    var origins = options.include || options.load;
    var condensed = condense.condense(origins, null, {sortOutput: true});
    var out = JSON.stringify(condensed, null, 2);
    var expect = fs.readFileSync(conf.jsonFile(origins[0]), "utf8").trim();
    if (out != expect)
      return util.failure("condense/" + origins[0] + ": Mismatch in condense output.\nGot " +
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

function jsFile(f) {
  return f + ".js";
}
exports.testConf = function(condenseConf,filter,options) {
  if (typeof options == "string") options = {load: [options]};
  options.load = options.load.map(jsFile);
  if (options.include) options.include = options.include.map(jsFile);
  if (filter && options.load[0].indexOf(filter) == -1) return;
  util.addTest();
  util.addFile();
  exports.runTest(condenseConf,options);
}