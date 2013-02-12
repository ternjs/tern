(function(exports) {
  var tern;
  if (typeof require != "undefined") {
    tern = require("./infer");
  } else {
    tern = exports;
  }

  function pathLen(path) {
    var len = 0, pos = 0, slash;
    while ((slash = path.indexOf("/", pos)) != -1) {
      ++len;
      pos = slash + 1;
      if (path.charAt(pos) == "!") len += .1;
    }
    return len;
  }

  function setPath(type, path) {
    var actual = type.getType();
    if (!actual || actual.path && pathLen(actual.path) <= pathLen(path)) return;
    actual.setPath(path);
  }

  tern.Prim.prototype.setPath = function() {};

  tern.Arr.prototype.setPath = function(path) {
    this.path = path;
    setPath(this.getProp("<i>"), path + "/<i>");
  };

  tern.Fn.prototype.setPath = function(path) {
    tern.Obj.prototype.setPath.call(this, path);
    for (var i = 0; i < this.args.length; ++i) setPath(this.args[i], path + "/!" + i);
    setPath(this.retval, path + "/!ret");
  };

  tern.Obj.prototype.setPath = function(path) {
    this.path = path || "/";
    for (var prop in this.props)
      setPath(this.props[prop], path + "/" + prop);
    if (this.proto) setPath(this.proto, path + "/!proto");
  };

  function desc(type, state) {
    var actual = type.getType();
    if (!actual) return "?";
    var found = type.path && state.paths[type.path];
    if (found) return found.name;
    if (state.seen.indexOf(type) > -1) return "?";
    state.seen.push(type);
    var d = actual.getDesc(state);
    state.seen.pop();
    return d;
  }

  tern.Prim.prototype.getDesc = function() { return this.name; };

  tern.Arr.prototype.getDesc = function(state) {
    return "[" + desc(this.getProp("<i>"), state) + "]";
  };

  tern.Fn.prototype.getDesc = function(state) {
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
      if (!obj) obj = out = {"!type": out};
      obj[p] = desc(this.props[p], state);
    }
    return out;
  };

  function setProps(source, target, state) {
    var sawProp = false;
    for (var prop in source.props) {
      var val = source.props[prop];
      if (val.flags & tern.flag_definite) {
        target[prop] = desc(val, state);
        sawProp = true;
      }
    }
    return sawProp;
  }

  tern.Obj.prototype.getDesc = function(state) {
    if (state.sources.indexOf(this.origin) == -1) return this.path;
    if (this._fromProto) return this.proto.path;

    var known = state.paths[this.path];
    if (known) {
      known.refs++;
      return this.path;
    }

    var structure = {};
    if (this.proto && this.proto != state.cx.protos.Object) {
      var proto = desc(this.proto, state);
      if (this.proto.name && /\.prototype$/.test(this.proto.name) &&
          !/\.prototype$/.test(this.name)) {
        this._fromProto = true;
        setProps(this, state.paths[this.proto.path].structure, state);
        return this.proto.path;
      }
      structure["!proto"] = proto;
    }
    if (setProps(this, structure, state) || (this.proto && this.proto != state.cx.protos.Object)) {
      state.paths[this.path] = {refs: 1, structure: structure};
      return this.path;
    } else {
      return "?";
    }
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

    for (var v in desc) if (v != "!types" && v != "!name")
      desc[v] = sanitize(desc[v], state);
    return desc;
  }

  exports.condense = function(sources, name) {
    if (typeof sources == "string") sources = [sources];
    if (!name) name = sources[0];

    var cx = tern.cx(), types = {}, haveType = false;
    var output = {"!name": name, "!types": types};
    var state = {sources: sources,
                 paths: Object.create(null),
                 cx: cx,
                 seen: []};

    setPath(cx.topScope, "");
    for (var v in cx.topScope.props) {
      var typ = cx.topScope.props[v].getType();
      if (typ && sources.indexOf(typ.origin) > -1)
        output[v] = desc(typ, state);
    }

    for (var path in state.paths) sanitize(state.paths[path].structure, state);
    sanitize(output, state);

    for (var path in state.paths) {
      var obj = state.paths[path];
      if (obj.inlined) continue;
      types[path] = obj.structure;
      haveType = true;
    }
    if (!haveType) delete output["!types"];

    return output;
  };
})(typeof exports == "undefined" ? window.tern : exports);
