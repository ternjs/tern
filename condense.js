(function(exports) {
  var tern;
  if (typeof require != "undefined") {
    tern = require("./infer");
  } else {
    tern = exports;
  }

  function desc(type, state) {
    var actual = type.getType();
    if (!actual) return "?";
    if (state.seen.indexOf(type) > -1) return (type.name && state.tags[type.name]) || "?";
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
    for (var prop in source.props) {
      var val = source.props[prop];
      if (val.flags & tern.flag_definite)
        target[prop] = desc(val, state);
    }
  }

  tern.Obj.prototype.getDesc = function(state) {
    if (state.sources.indexOf(this.origin) == -1) return this.originTag() || "?";
    if (this._fromProto) return this.proto.name;

    if (!this.name)
      this.name = "__obj" + (state.objId++);

    var known = state.tags[this.name];
    if (known) {
      known.refs++;
    } else {
      var structure = {}, proto;
      state.tags[this.name] = {refs: 1, structure: structure, obj: this};
      if (this.proto && this.proto != state.cx.protos.Object) {
        proto = desc(this.proto, state);
        if (this.proto.name && /\.prototype$/.test(this.proto.name) &&
            !/\.prototype$/.test(this.name)) {
          this._fromProto = true;
          setProps(this, state.tags[this.proto.name].structure, state);
          return this.proto.name;
        }
      }
      if (proto) structure["!proto"] = proto;
      setProps(this, structure, state);
    }
    return "%" + this.name + "%";
  };

  function sanitize(desc, state) {
    if (typeof desc == "string") return sanitizeString(desc, state);

    for (var v in desc) if (v != "!types" && v != "!name")
      desc[v] = sanitize(desc[v], state);
    return desc;
  }

  function sanitizeString(desc, state) {
    if (/^%[^%]+%$/.test(desc)) {
      var tag = desc.slice(1, -1), obj = state.tags[tag];
      if (obj.refs == 1) {
        obj.inlined = true;
        return obj.structure;
      } else return tag;
    }

    return desc.replace(/%([^%]+)%/g, function(m, tag) { return tag; });
  }

  exports.condense = function(sources, name) {
    if (typeof sources == "string") sources = [sources];
    if (!name) name = sources[0];

    var cx = tern.cx(), types = {}, haveType = false;
    var output = {"!name": name, "!types": types};
    var state = {sources: sources,
                 tags: Object.create(null),
                 cx: cx,
                 seen: [],
                 objId: 0};

    for (var v in cx.topScope.props) {
      var typ = cx.topScope.props[v].getType();
      if (typ && sources.indexOf(typ.origin) > -1)
        output[v] = desc(typ, state);
    }

    for (var tag in state.tags) sanitize(state.tags[tag].structure, state);
    sanitize(output, state);

    for (var tag in state.tags) {
      var obj = state.tags[tag];
      if (obj.inlined) continue;
      types[tag] = obj.structure;
      haveType = true;
    }
    if (!haveType) delete output["!types"];

    return output;
  };
})(typeof exports == "undefined" ? window.tern : exports);
