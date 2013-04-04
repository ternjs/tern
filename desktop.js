// HTTP server for desktop editors

// Reads .tern-project files, wraps a Tern server in an HTTP wrapper
// so that editor plug-ins can talk to it.

var tern = require("./tern");
var fs = require("fs"), path = require("path"), url = require("url");

var projectFileName = ".tern-project", portFileName = ".tern-port";
var maxIdleTime = 6e4 * 5; // Shut down after five minutes of inactivity

var persistent = process.argv.indexOf("--persistent") > -1;

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
  libs: [],
  loadEagerly: false,
  plugins: {},
  ecmaScript: true
};

function readProjectFile(dir) {
  var data = JSON.parse(fs.readFileSync(path.resolve(dir, projectFileName), "utf8"));
  for (var option in defaultConfig) if (!data.hasOwnProperty(option))
    data[option] = defaultConfig[option];
  return data;
}

function findFile(file, projectDir, fallbackDir) {
  var local = path.resolve(projectDir, file);
  if (fs.existsSync(local)) return local;
  var shared = path.resolve(fallbackDir, file);
  if (fs.existsSync(shared)) return shared;
}

function findDefs(projectDir, config) {
  var defs = [], src = config.libs;
  if (src.indexOf("ecma5") == -1 && config.ecmaScript) src = ["ecma5"].concat(src);
  for (var i = 0; i < src.length; ++i) {
    var file = src[i];
    if (!/\.json$/.test(file)) file = file + ".json";
    var found = findFile(file, projectDir, __dirname + "/defs");
    if (found) defs.push(JSON.parse(fs.readFileSync(found, "utf8")));
    else process.stderr.write("Failed to find library " + src[i] + ".\n");
  }
  return defs;
}

function loadPlugins(projectDir, plugins) {
  var options = {};
  for (var plugin in plugins) {
    var found = findFile(plugin + ".js", projectDir, __dirname + "/plugin");
    if (!found) {
      process.stderr.write("Failed to find plugin " + plugin + ".\n");
      continue;
    }
    require(found);
    options[path.basename(plugin)] = plugins[plugin];
  }
  return options;
}

var projectDir = findProjectDir();
if (projectDir) {
  var config = readProjectFile(projectDir);
} else {
  projectDir = process.cwd();
  var config = defaultConfig;
}
var server = startServer(projectDir, config);

function startServer(dir, config) {
  var defs = findDefs(dir, config);
  var plugins = loadPlugins(dir, config.plugins);
  var server = new tern.Server({
    getFile: function(name, c) {
      fs.readFile(path.resolve(dir, name), "utf8", c);
    },
    defs: defs,
    plugins: plugins,
    debug: true,
    async: true,
    projectDir: dir
  });
  // FIXME maybe allow globs?
  if (config.loadEagerly) config.loadEagerly.forEach(function(file) {
    server.addFile(file);
  });
  return server;
}

function doShutdown() {
  if (persistent) return;
  console.log("Was idle for " + Math.floor(maxIdleTime / 6e4) + " minutes. Shutting down.");
  process.exit();
}

var shutdown = setTimeout(doShutdown, maxIdleTime);

var httpServer = require("http").createServer(function(req, resp) {
  clearTimeout(shutdown);
  shutdown = setTimeout(doShutdown, maxIdleTime);

  var target = url.parse(req.url, true);
  if (target.pathname == "/ping") return respondSimple(resp, 200, "pong");
  if (target.pathname != "/") return respondSimple(resp, 404, "No service at " + target.pathname);

  if (req.method == "POST") {
    var body = "";
    req.on("data", function (data) { body += data; });
    req.on("end", function() { respond(resp, body); });
  } else if (req.method == "GET") {
    if (target.query.doc) respond(resp, target.query.doc);
    else respondSimple(resp, 400, "Missing query document");
  }
});
httpServer.listen(0, "localhost", function() {
  var portFile = path.resolve(projectDir, portFileName), port = httpServer.address().port;
  fs.writeFileSync(portFile, String(port), "utf8");
  process.on("exit", function() {
    try {
      var cur = Number(fs.readFileSync(portFile, "utf8"));
      if (cur == port) fs.unlinkSync(portFile);
    } catch(e) {}
  });
  process.on("SIGINT", function() { process.exit(); });
  console.log("Listening on port " + port);
});

function respondSimple(resp, status, text) {
  resp.writeHead(status, {"content-type": "text/plain"});
  resp.end(text);
}

function respond(resp, doc) {
  try { var doc = JSON.parse(doc); }
  catch(e) { return respondSimple(resp, 400, "JSON parse error: " + e.message); }

  server.request(doc, function(err, data) {
    if (err) return respondSimple(resp, 400, String(err));
    resp.writeHead(200, {"content-type": "application/json"});
    resp.end(JSON.stringify(data));
  });
}
