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
    if (slash >= 0) path = base.slice(0, slash + 1) + path;
    while (m = /[^\/]*[^\/\.][^\/]*\/\.\.\//.exec(path))
      path = path.slice(0, m.index) + path.slice(m.index + m[0].length);
    return path.replace(/(^|[^\.])\.\//g, "$1");
  }

  function buildWrappingScope(parent, origin) {
    var scope = new infer.Scope(parent);
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

  infer.registerFunction("nodeRequire", function(_self, _args, argNodes) {
    if (!argNodes || !argNodes.length || argNodes[0].type != "Literal" || typeof argNodes[0].value != "string")
      return infer.ANull;
    var cx = infer.cx(), data = cx.parent._node, name = argNodes[0].value;
    var node = cx.topScope.getProp("node").getType(), val;
    if (name != "Module" && node.props && (val = node.props[name]))
      return val;

    if (/^\.{0,2}\//.test(name)) { // Relative
      if (!/\.[^\/]*$/.test(name)) name = name + ".js";
      name = resolvePath(data.currentFile, name);
      cx.parent.addFile(name);
      return getModule(data, name);
    }

    if (name in data.modules) return data.modules[name];

    if (data.options.modules && data.options.modules.hasOwnProperty(name)) {
      var scope = buildWrappingScope(cx.topScope, name);
      infer.env.loadEnvironment(data.options.modules[name], scope);
      return data.modules[name] = exportsFromScope(scope);
    }

    return infer.ANull;
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
      file.scope = buildWrappingScope(file.scope, file.name);
    });

    server.on("afterLoad", function(file) {
      exportsFromScope(file.scope).propagate(getModule(this._node, this._node.currentFile));
    });

    server.on("reset", function(file) {
      this._node.modules = Object.create(null);
    });
  });
});
