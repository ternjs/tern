(function(exports) {
  var infer;
  if (typeof require != "undefined") {
    infer = require("./infer");
  } else {
    infer = exports;
  }

  function pathLen(path) {
    var len = 1, pos = 0, dot;
    while ((dot = path.indexOf(".", pos)) != -1) {
      ++len;
      pos = dot + 1;
      if (path.charAt(pos) == "!") len += .1;
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
      if (name && typeof name != "string") name = name.name;
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

    var obj;
    for (var p in this.props) {
      if (!obj) {
        var known = state.paths[this.path];
        if (known) {
          known.refs++;
          return this.path;
        } else {
          obj = {"!type": out};
        }
      }
      obj[p] = desc(this.props[p], state);
    }
    if (obj) {
      state.paths[this.path] = {refs: 1, structure: obj};
      return this.path;
    }
    return out;
  };

  function hasProps(obj) {
    for (var prop in obj.props) {
      var val = obj.props[prop];
      if (val.flags & infer.flag_definite) return true;
    }
  }

  function setProps(source, target, state) {
    for (var prop in source.props) {
      var val = source.props[prop];
      if (val.flags & infer.flag_definite)
        target[prop] = desc(val, state);
    }
  }

  infer.Obj.prototype.getDesc = function(state, flag) {
    if (!isTarget(state, this)) return this.path;
    if (this._fromProto) return "+" + this.proto.path;

    var known = state.paths[this.path];
    if (known) {
      known.refs++;
      return this.path;
    }
    var structure = {}, proto;
    state.paths[this.path] = {refs: 1, structure: structure};

    if (this.proto && this.proto != state.cx.protos.Object) {
      if (this.proto.name && /\.prototype$/.test(this.proto.name) &&
          !/\.prototype$/.test(this.name)) {
        proto = desc(this.proto, state, "force");
        this._fromProto = true;
        delete state.paths[this.path];
        var protoDesc = state.paths[this.proto.path];
        if (protoDesc) setProps(this, protoDesc.structure, state);
        return "+" + this.proto.path;
      } else {
        proto = desc(this.proto, state);
        if (proto == "?") proto = null;
      }
    }
    if (flag != "force" && !proto && !hasProps(this)) return "?";
    if (proto) structure["!proto"] = proto;
    setProps(this, structure, state);
    return this.path;
  };

  function sanitize(desc, state) {
    if (typeof desc == "string") {
      var found = state.paths[desc];
      if (found && found.refs == 1) {
        found.inlined = true;
        return found.structure;
      }
      return desc;
    }

    for (var v in desc) if (v != "!predef" && v != "!name")
      desc[v] = sanitize(desc[v], state);
    return desc;
  }

  exports.condense = function(sources, name) {
    if (typeof sources == "string") sources = [sources];
    if (!name) name = sources[0];

    var cx = infer.cx(), predef = {}, minOrigin = Infinity;
    for (var i = 0; i < sources.length; ++i)
      minOrigin = Math.min(cx.origins.indexOf(sources[i]), minOrigin);
    var output = {"!name": name, "!predef": predef};
    var state = {sources: sources,
                 paths: Object.create(null),
                 cx: cx,
                 minOrigin: minOrigin,
                 addedToForeign: [],
                 seen: []};

    setPath(cx.topScope, "", state, 0);

    for (var v in cx.topScope.props) {
      var typ = cx.topScope.props[v].getType(false);
      if (typ && sources.indexOf(typ.origin) > -1)
        output[v] = desc(typ, state);
    }
    if (state.addedToForeign.length > 0) {
      var foreign = {}, haveForeign = false, list = state.addedToForeign;
      state.addedToForeign = [];
      for (var i = 0; i < list.length; ++i) {
        var val = desc(list[i], state);
        if (val != "?") haveForeign = foreign[list[i].path] = val;
      }
      if (haveForeign) output["!added"] = foreign;
    }

    for (var path in state.paths) sanitize(state.paths[path].structure, state);
    sanitize(output, state);

    var haveType = false;
    for (var path in state.paths) {
      var obj = state.paths[path];
      if (obj.inlined) continue;
      predef[path] = obj.structure;
      haveType = true;
    }
    if (!haveType) delete output["!predef"];

    return output;
  };
})(typeof exports == "undefined" ? window.tern : exports);
