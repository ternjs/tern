(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    return mod(require("../../infer"), require("../../tern"));
  if (typeof define == "function" && define.amd) // AMD
    return define(["../../infer", "../../tern"], mod);
  mod(tern, tern);
})(function(infer, tern) {
  "use strict";

  function resolveName(name, data) {
    var opts = data.options;
    var base = opts.baseURL || "";
    if (!opts.paths) return base + "/" + name + ".js";
    var known = opts.paths[name];
    if (known) return base + "/" + known + ".js";
    var dir = name.match(/^([^\/]+)(\/.*)$/);
    if (dir) {
      var known = opts.paths[dir[0]];
      if (known) return base + "/" + known + dir[1] + ".js";
    }
    return base + "/" + name + ".js";
  }

  function flattenPath(path) {
    if (!/(^|\/)(\.\/|[^\/]+\/\.\.\/)/.test(path)) return path;
    var parts = path.split("/");
    for (var i = 0; i < parts.length; ++i) {
      if (parts[i] == ".") parts.splice(i--, 1);
      else if (i && parts[i] == "..") parts.splice(i-- - 1, 2);
    }
    return parts.join("/");
  }

  function getInterface(name, data) {
    if (!/^(https?:|\/)|\.js$/.test(name))
      name = resolveName(name, data);
    name = flattenPath(name);
    var known = data.interfaces[name];
    if (!known) {
      known = data.interfaces[name] = new infer.AVal;
      data.server.addFile(name);
    }
    return known;
  }

  infer.registerFunction("requireJS", function(_self, args, argNodes) {
    var server = infer.cx().parent, data = server && server._requireJS;
    if (!data || !args.length) return infer.ANull;

    var deps = [];
    if (argNodes && args.length > 1) {
      var node = argNodes[args.length == 2 ? 0 : 1];
      if (node.type == "Literal" && typeof node.value == "string") {
        deps.push(getInterface(node.value, data));
      } else if (node.type == "ArrayExpression") for (var i = 0; i < node.elements.length; ++i) {
        var elt = node.elements[i];
        if (elt.type == "Literal" && typeof elt.value == "string") deps.push(getInterface(elt.value, data));
      }
    }

    var value = args[Math.min(args.length - 1, 2)];
    if (value.isEmpty() || value.getFunctionType()) {
      var retval = new infer.AVal;
      value.propagate(new infer.IsCallee(infer.ANull, deps, null, retval));
      value = retval;
    }
    var name = data.currentFile;
    var known = data.interfaces[name];
    if (!known) known = data.interfaces[name] = new infer.AVal;
    value.propagate(known);

    return infer.ANull;
  });

  tern.registerPlugin("requireJS", function(server, options) {
    server._requireJS = {
      interfaces: Object.create(null),
      options: options || {},
      currentFile: null,
      server: server
    };

    server.on("beforeLoad", function(file) {
      this._requireJS.currentFile = file.name;
    });
    server.on("reset", function(file) {
      this._requireJS.interfaces = Object.create(null);
    });
  });
});
