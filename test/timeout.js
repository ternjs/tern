var tern = require("../lib/tern");
var infer = require("../lib/infer");
var util = require("./util");
var fs = require("fs");

exports.runTests = function(filter) {
  if (filter && "timeout".indexOf(filter) == -1) return;

  util.addFile();
  util.addTest();
  var server = new tern.Server({});
  var file = fs.readFileSync(util.resolve("lib/infer.js"), "utf8");
  try {
    server.request({timeout: 10,
                    files: [{type: "full", name: "infer.js", text: file}],
                    query: {type: "type", end: 0, file: "infer.js"}}, function(err, result) {
      if (!err || !(err instanceof infer.TimedOut))
        util.failure("timeout: failed to time out");
    })
  } catch(e) {
    util.failure("timeout: exception thrown: " + e.stack);
  }
};
