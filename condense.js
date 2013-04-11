// Condensing an inferred set of types to a JSON description document.

// This code can be used to, after a library has been analyzed,
// extract the types defined in that library and dump them as a JSON
// structure (as parsed by def.js).

// The idea being that big libraries can be analyzed once, dumped, and
// then cheaply included in later analysis.

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    return mod(exports, require("./infer"));
  if (typeof define == "function" && define.amd) // AMD
    return define(["exports", "./infer"], mod);
  mod(self.tern || (self.tern = {}), tern); // Plain browser env
})(function(exports, infer) {
  "use strict";

  function pathLen(path) {
    var len = 1, pos = 0, dot;
    while ((dot = path.indexOf(".", pos)) != -1) {
      pos = dot + 1;
      len += path.charAt(pos) == "!" ? 10 : 1;
    }
    return len;
  }

  function isTarget(state, type) {
    return state.sources.indexOf(type.origin) > -1;
  }

  function setPath(type, path, state, maxOrigin) {
    var actual = type.getType(false);
    if (!actual) return;
    if (actual.origin) {
      var origPos = state.cx.origins.indexOf(actual.origin);
      if (origPos < maxOrigin) return;
      if (maxOrigin < state.minOrigin && isTarget(state, actual) && path.indexOf("!") < 0 &&
          state.addedToForeign.indexOf(actual) < 0)
        state.addedToForeign.push(actual);
      maxOrigin = origPos;
    }
    if (actual.path && pathLen(actual.path) <= pathLen(path)) return;
    actual.setPath(path, state, maxOrigin);
  }

  infer.Prim.prototype.setPath = function() {};

  infer.Arr.prototype.setPath = function(path, state, maxOrigin) {
    this.path = path;
    setPath(this.getProp("<i>"), path + ".<i>", state, maxOrigin);
  };

  infer.Fn.prototype.setPath = function(path, state, maxOrigin) {
    infer.Obj.prototype.setPath.call(this, path, state, maxOrigin);
    for (var i = 0; i < this.args.length; ++i) setPath(this.args[i], path + ".!" + i, state, maxOrigin);
    setPath(this.retval, path + ".!ret", state, maxOrigin);
  };

  infer.Obj.prototype.setPath = function(path, state, maxOrigin) {
    this.path = path || "<top>";
    var start = path ? path + "." : "";
    for (var prop in this.props)
      setPath(this.props[prop], start + prop, state, maxOrigin);
    if (this.proto) setPath(this.proto, start + "!proto", state, maxOrigin);
  };

  // FIXME maybe cut off output at a certain path length? the long
  // paths tend to be uninteresting internals
  function desc(type, state, flag) {
    var actual = type.getType(false);
    if (!actual) return "?";
    var inForeign = state.addedToForeign.indexOf(actual);
    if (inForeign >= 0) state.addedToForeign.splice(inForeign, 1);

    var found = type.path && state.paths[type.path];
    if (found) return type.path;

    if (state.seen.indexOf(type) > -1) return type.path || "?";
    state.seen.push(type);
    var d = actual.getDesc(state, flag);
    state.seen.pop();
    return d;
  }

  infer.Prim.prototype.getDesc = function() { return this.name; };

  infer.Arr.prototype.getDesc = function(state) {
    return "[" + desc(this.getProp("<i>"), state) + "]";
  };

  infer.Fn.prototype.getDesc = function(state) {
    if (this.path && !isTarget(state, this)) return this.path;

    var out = "fn(";
    for (var i = 0; i < this.args.length; ++i) {
      if (i) out += ", ";
      var name = this.argNames[i];
      if (name && name != "?") out += name + ": ";
      out += desc(this.args[i], state);
    }
    out += ")";
    if (this.computeRetSource) {
      out += " -> " + this.computeRetSource;
    } else if (!this.retval.isEmpty()) {
      var rettype = this.retval.getType();
      if (rettype) out += " -> " + desc(rettype, state);
    }

    if (!hasProps(this)) return out;

    var obj = {"!type": out};
    state.paths[this.path] = {structure: obj};
    setProps(this, obj, state);
    return this.path;
  };

  function hasProps(obj) {
    for (var prop in obj.props) return true;
  }

  function setProps(source, target, state) {
    for (var prop in source.props) {
      var val = source.props[prop];
      if (isTarget(state, val)) target[prop] = desc(val, state);
    }
  }

  function protoName(o) {
    return /\.prototype$/.test(o.name) || /\.prototype$/.test(o.path);
  }

  function isSimpleInstance(o, state) {
    if (o._fromProto) return true;

    if (o.proto && o.proto != state.cx.protos.Object &&
        o.proto.name && protoName(o.proto) && !protoName(o)) {
      desc(o.proto, state, "force");
      o._fromProto = true;
      var protoDesc = state.paths[o.proto.path];
      if (protoDesc) setProps(o, protoDesc.structure, state);
      return true;
    }
  }

  infer.Obj.prototype.getDesc = function(state, flag) {
    if (!isTarget(state, this)) return this.path;
    if (isSimpleInstance(this, state)) return "+" + this.proto.path.replace(/\.prototype$/, "");

    var structure = {}, proto;
    state.paths[this.path] = {structure: structure};

    if (this.proto && this.proto != state.cx.protos.Object) {
      proto = desc(this.proto, state);
      if (proto == "?") proto = null;
    }
    if (flag != "force" && !proto && !hasProps(this)) return "?";
    if (proto) structure["!proto"] = proto;
    setProps(this, structure, state);
    return this.path;
  };

  function sanitize(desc, state, path) {
    if (typeof desc == "string") {
      var found;
      if (desc == path && (found = state.paths[desc])) {
        found.inlined = true;
        return sanitize(found.structure, state, path);
      } else {
        return desc;
      }
    }

    for (var v in desc) if (v != "!define" && v != "!name")
      desc[v] = sanitize(desc[v], state, path == null ? null : path ? path + "." + v : v);
    return desc;
  }

  exports.condense = function(sources, name) {
    if (typeof sources == "string") sources = [sources];
    if (!name) name = sources[0];

    var cx = infer.cx(), defs = {}, minOrigin = Infinity;
    for (var i = 0; i < sources.length; ++i)
      minOrigin = Math.min(cx.origins.indexOf(sources[i]), minOrigin);
    var output = {"!name": name, "!define": defs};
    var state = {sources: sources,
                 paths: Object.create(null),
                 cx: cx,
                 minOrigin: minOrigin,
                 addedToForeign: [],
                 seen: []};

    setPath(cx.topScope, "", state, 0);

    for (var v in cx.topScope.props) {
      var av = cx.topScope.props[v];
      if (!isTarget(state, av)) continue;
      var typ = av.getType(false);
      if (typ && isTarget(state, typ)) output[v] = desc(typ, state);
    }

    if (state.addedToForeign.length > 0) {
      var list = state.addedToForeign;
      state.addedToForeign = [];
      for (var i = 0; i < list.length; ++i) {
        var d = list[i], parts = d.path.split(".");
        var parent = infer.def.parsePath(parts.slice(0, parts.length - 1).join("."));
        if (isSimpleInstance(parent, state))
          parts = parent.proto.path.split(".").concat(parts[parts.length - 1]);
        var val = desc(list[i], state);
        if (val == "?" || parts.some(function(s) {return s.charAt(0) == "!";})) continue;
        for (var j = 0, cur = output; j < parts.length - 1; ++j) {
          var part = parts[j];
          if (Object.prototype.hasOwnProperty.call(cur, part)) cur = cur[part];
          else cur = cur[part] = {};
        }
        cur[parts[parts.length - 1]] = val;
      }
    }

    sanitize(output, state, "");
    var haveDef = false;
    for (var path in state.paths) {
      var elt = state.paths[path];
      if (!elt.inlined) haveDef = defs[path] = sanitize(elt.structure, state, null);
    }
    if (!haveDef) delete output["!define"];

    return output;
  };
});
