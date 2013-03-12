// HTTP server for desktop editors

// Reads .tern-project files, wraps a Tern server in an HTTP wrapper
// so that editor plug-ins can talk to it.

var tern = require("./tern");
var fs = require("fs"), path = require("path");

var projectFileName = ".tern-project";

function findProjectDir() {
  var dir = process.cwd();
  for (;;) {
    try {
      if (fs.statSync(path.resolve(dir, projectFileName)).isFile()) return dir;
    } catch(e) {}
    var end = dir.lastIndexOf("/");
    if (end <= 0) return null;
    dir = dir.slice(0, end);
  }
}

var defaultConfig = {
  environment: ["ecma5"],
  loadLibraries: []
};
var knownEnvironments = {
  ecma5: JSON.parse(fs.readFileSync(path.resolve(__dirname, "ecma5.json"), "utf8")),
  browser: JSON.parse(fs.readFileSync(path.resolve(__dirname, "browser.json"), "utf8"))
};

function readProjectFile(dir) {
  var data = JSON.parse(fs.readFileSync(path.resolve(dir, projectFileName), "utf8"));
  for (var option in defaultConfig) if (!data.hasOwnProperty(option))
    data[option] = defaultConfig[option];
  return data;
}

var dir = findProjectDir(), config;
if (dir) {
  config = readProjectFile(dir);
} else {
  dir = process.cwd();
  config = defaultConfig;
}
var server = startServer(dir, config);

function startServer(dir, config) {
  function getFile(name, c) {
    fs.readFile(path.resolve(dir, name), "utf8", c);
  }
  var env = [];
  config.environment.forEach(function(name) {
    env.push(knownEnvironments[name] || JSON.parse(fs.readFileSync(path.resolve(dir, name), "utf8")));
  });

  return new tern.Server({getFile: getFile, environment: env, debug: true});
}

var url = require("url");

var httpServer = require("http").createServer(function(req, resp) {
  var target = url.parse(req.url, true);
  if (target.path != "/") respondError(resp, 404, "No service at " + target.path);

  if (req.method == "POST") {
    var body = "";
    req.on("data", function (data) { body += data; });
    req.on("end", function() { respond(resp, body); });
  } else if (req.method == "GET") {
    if (target.query.doc) respond(resp, target.query.doc);
    else respondError(resp, 400, "Missing query document");
  }
});
httpServer.listen(0, "localhost", function() {
  fs.writeFileSync(path.resolve(dir, ".tern-port"), String(httpServer.address().port), "utf8");
});

function respondError(resp, status, text) {
  resp.writeHead(status, {"content-type": "text/plain"});
  resp.end(text);
}

function respond(resp, doc) {
  try { var doc = JSON.parse(doc); }
  catch(e) { return respondError(resp, 400, "JSON parse error: " + e.message); }

  server.request(doc, function(err, data) {
    if (err) return respondError(resp, 400, String(err));
    resp.writeHead(200, {"content-type": "application/json"});
    resp.end(JSON.stringify(data));
  });
}
