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
  loadEagerly: false, // FIXME implement
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

function buildEnvironment(projectDir, config) {
  var env = [], src = config.libs;
  if (src.indexOf("ecma5") == -1 && config.ecmaScript) src = ["ecma5"].concat(src);
  for (var i = 0; i < src.length; ++i) {
    var file = src[i];
    if (!/\.json$/.test(file)) file = file + ".json";
    var found = findFile(file, projectDir, __dirname + "/defs");
    if (found) env.push(JSON.parse(fs.readFileSync(found, "utf8")));
    else process.stderr.write("Failed to find library " + src[i] + ".\n");
  }
  return env;
}

function loadPlugins(projectDir, plugins, env) {
  var options = {};
  for (var file in plugins) {
    var found = findFile(file, projectDir, __dirname + "/plugin") ||
      findFile(file + ".js", projectDir, __dirname + "/plugin");
    if (!found) {
      process.stderr.write("Failed to find plugin " + file + ".\n");
      continue;
    }
    if (fs.statSync(found).isDirectory()) fs.readdirSync(found).forEach(function(file) {
      if (/\.js$/.test(file)) require(found + "/" + file);
      else if (/\.json$/.test(file)) env.push(JSON.parse(fs.readFileSync(found + "/" + file, "utf8")));
    });
    else require(found);
    options[path.basename(file, ".js")] = plugins[file];
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
  var env = buildEnvironment(dir, config);
  var plugins = loadPlugins(dir, config.plugins, env);
  return new tern.Server({
    getFile: function(name, c) {
      fs.readFile(path.resolve(dir, name), "utf8", c);
    },
    environment: env,
    pluginOptions: plugins,
    debug: true,
    async: true
  });
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
  if (target.path == "/ping") return respondSimple(resp, 200, "pong");
  if (target.path != "/") return respondSimple(resp, 404, "No service at " + target.path);

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
  var portFile = path.resolve(projectDir, portFileName);
  fs.writeFileSync(portFile, String(httpServer.address().port), "utf8");
  process.on("exit", function() { try { fs.unlinkSync(portFile); } catch(e) {} });
  process.on("SIGINT", function() { process.exit(); });
  console.log("Listening on port " + httpServer.address().port);
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
