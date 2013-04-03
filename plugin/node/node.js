(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    return mod(require("../../infer"), require("../../tern"), require);
  if (typeof define == "function" && define.amd) // AMD
    return define(["../../infer", "../../tern"], mod);
  mod(tern, tern);
})(function(infer, tern, require) {
  "use strict";

  function resolvePath(base, path) {
    var slash = base.lastIndexOf("/"), m;
    if (slash >= 0) path = base.slice(0, slash + 1) + path;
    while (m = /[^\/]*[^\/\.][^\/]*\/\.\.\//.exec(path))
      path = path.slice(0, m.index) + path.slice(m.index + m[0].length);
    return path.replace(/(^|[^\.])\.\//g, "$1");
  }

  function getModule(data, name) {
    return data.modules[name] || (data.modules[name] = new infer.AVal);
  }

  function buildWrappingScope(parent, origin, node) {
    var scope = new infer.Scope(parent);
    scope.node = node;
    infer.env.parsePath("node.require").propagate(scope.defProp("require"));
    var module = infer.getInstance(infer.env.parsePath("node.Module.prototype").getType());
    module.propagate(scope.defProp("module"));
    var exports = new infer.Obj(true, "exports", origin);
    exports.propagate(scope.defProp("exports"));
    exports.propagate(module.defProp("exports"));
    return scope;
  }

  function exportsFromScope(scope) {
    var exportsVal = scope.getProp("module").getType().getProp("exports");
    if (!(exportsVal instanceof infer.AVal))
      return file.scope.getProp("exports");
    else
      return exportsVal.types[exportsVal.types.length - 1];
  }

  function resolveModule() { return infer.ANull; }

  // Assume node.js & access to local file system
  if (require) (function() {
    var fs = require("fs"), path = require("path");

    function findModuleDir(server) {
      if (server._node.moduleDir !== undefined) return server._node.moduleDir;

      for (var dir = server.options.projectDir || "";;) {
        var modDir = path.resolve(dir, "node_modules");
        try {
          if (fs.statSync(modDir).isDirectory()) return server._node.moduleDir = modDir;
        } catch(e) {}
        var end = dir.lastIndexOf("/");
        if (end <= 0) return server._node.moduleDir = null;
        dir = dir.slice(0, end);
      }
    }

    resolveModule = function(server, name) {
      var modDir = findModuleDir(server);
      if (!modDir) return infer.ANull;

      var file = name;
      if (name.indexOf("/") < 0) {
        try {
          var pkg = JSON.parse(fs.readFileSync(path.resolve(modDir, name + "/package.json")));
        } catch(e) { return infer.ANull; }
        file = name + "/" + pkg.main;
      }
      if (!/\.js$/.test(file)) file += ".js";

      file = path.resolve(modDir, file);
      if (!fs.existsSync(file)) return infer.ANull;
      server.addFile(file);
      return server._node.modules[file] = server._node.modules[name] = new infer.AVal;
    };
  })();

  infer.registerFunction("nodeRequire", function(_self, _args, argNodes) {
    if (!argNodes || !argNodes.length || argNodes[0].type != "Literal" || typeof argNodes[0].value != "string")
      return infer.ANull;
    var cx = infer.cx(), server = cx.parent, data = server._node, name = argNodes[0].value;
    var node = cx.topScope.getProp("node").getType(), val;
    if (name != "Module" && node.props && (val = node.props[name]))
      return val;

    if (/^\.{0,2}\//.test(name)) { // Relative
      if (!data.currentFile) return argNodes[0].required || infer.ANull;
      if (!/\.[^\/]*$/.test(name)) name = name + ".js";
      name = resolvePath(data.currentFile, name);
      server.addFile(name);
      return argNodes[0].required = getModule(data, name);
    }

    if (name in data.modules) return data.modules[name];

    if (data.options.modules && data.options.modules.hasOwnProperty(name)) {
      var scope = buildWrappingScope(cx.topScope, name);
      infer.env.loadEnvironment(data.options.modules[name], scope);
      return data.modules[name] = exportsFromScope(scope);
    } else {
      return resolveModule(server, name);
    }
  });

  tern.registerPlugin("node", function(server, options) {
    server._node = {
      modules: Object.create(null),
      options: options || {},
      currentFile: null,
      server: server
    };

    server.on("beforeLoad", function(file) {
      this._node.currentFile = file.name;
      file.scope = buildWrappingScope(file.scope, file.name, file.ast);
    });

    server.on("afterLoad", function(file) {
      this._node.currentFile = null;
      exportsFromScope(file.scope).propagate(getModule(this._node, file.name));
    });

    server.on("reset", function(file) {
      this._node.modules = Object.create(null);
    });
  });
});
