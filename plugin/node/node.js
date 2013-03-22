(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    return mod(require("../../infer"), require("../../tern"));
  if (typeof define == "function" && define.amd) // AMD
    return define(["../../infer", "../../tern"], mod);
  mod(tern, tern);
})(function(infer, tern) {
  "use strict";

  function getModule(data, name) {
    return data.modules[name] || (data.modules[name] = new infer.AVal);
  }

  function resolvePath(base, path) {
    var slash = base.lastIndexOf("/"), m;
    if (slash >= 0) base = base.slice(0, slash + 1);
    path = base + path;
    while (m = /[^\/]*[^\/\.][^\/]*\/\.\.\//.exec(path))
      path = path.slice(0, m.index) + path.slice(m.index + m[0].length);
    return path.replace(/(^|[^\.])\.\//g, "$1");
  }

  function resolveName(data, name) {
    if (!/\.[^\/]*$/.test(name)) name = name + ".js";
    if (/^\.\.?\//.test(name)) return resolvePath(data.currentFile, name);
    if (data.options.modulePaths) for (var i = 0; i < data.options.modulePaths.length; ++i) {
      var path = data.options.modulePath + name;
      if (path in data.modules || data.server.probeFile(path)) return path;
    }
  }

  infer.registerFunction("nodeRequire", function(_self, _args, argNodes) {
    if (!argNodes || !argNodes.length || argNodes[0].type != "Literal" || typeof argNodes[0].value != "string")
      return infer.ANull;
    var cx = infer.cx(), data = cx.parent._node, name = argNodes[0].value;
    var node = cx.topScope.getVar("node").getType(), val;
    if (name != "Module" && node.props && (val = node.props[name]) && val.flags & infer.flag_definite)
      return val;

    name = resolveName(data, name);
    if (!name) return infer.ANull;
    cx.parent.require(name);
    return getModule(data, name);
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
      file.scope = new infer.Scope(file.scope);

      infer.env.parsePath("node.require").propagate(file.scope.ensureProp("require"));
      var module = infer.getInstance(infer.env.parsePath("node.Module.prototype").getType());
      module.propagate(file.scope.ensureProp("module"));
      var exports = new infer.Obj(true, "exports", file.name);
      exports.propagate(file.scope.ensureProp("exports"));
      exports.propagate(module.ensureProp("exports"));
    });

    server.on("afterLoad", function(file) {
      var exportsVal = file.scope.getVar("module").getType().getProp("exports"), exports;
      if (!(exportsVal instanceof infer.AVal))
        exports = file.scope.getVar("exports");
      else
        exports = exportsVal.types[exportsVal.types.length - 1];
      exports.propagate(getModule(this._node, this._node.currentFile));
    });

    server.on("reset", function(file) {
      this._node.modules = Object.create(null);
    });
  });
});
