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
    return actual.getDesc(state);
  }

  tern.Prim.prototype.getDesc = function() { return this.name; };

  tern.Arr.prototype.getDesc = function(state) {
    return "[" + desc(type.getProp("<i>"), state) + "]";
  };

  tern.Fn.prototype.getDesc = function(state) {
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
    } else {
      var rettype = this.retval.getType();
      if (rettype) out += " -> " + rettype.getDesc(state);
    }
    return out;
  };

  tern.Obj.prototype.getDesc = function(state) {
    if (state.sources.indexOf(this.origin) == -1) return this.originTag() || "?";

    if (!this.name)
      this.name = "__obj" + Math.floor(Math.random(0xffffffff)).toString(16);

    var known = state.tags[this.name];
    if (known) {
      known.refs++;
    } else {
      var structure = {};
      state.tags[this.name] = {refs: 1, structure: structure};
      if (this.proto && this.proto != state.cx.protos.Object)
        structure["!proto"] = this.proto.getDesc(state);
      for (var prop in this.props) {
        var val = this.props[prop];
        if (val.flags & tern.flag_definite)
          structure[prop] = desc(val, state);
      }
    }
    return "%" + this.name + "%";
  };

  function sanitize(desc, state) {
    if (typeof desc == "string") return sanitizeString(desc, state);

    for (var v in desc) if (v.charAt(0) != "!")
      desc[v] = sanitize(desc[v], state);
    return desc;
  }

  function sanitizeString(desc, state) {
    if (/^%[^%]+%$/.test(desc)) {
      var tag = desc.slice(1, -1), obj = state.tags[tag];
      if (obj.refs == 1) return obj.structure;
      else return tag;
    }

    return desc.replace(/%([^%]+)%/g, function(m, tag) {
      var obj = state.tags[tag];
      // Must serialize the obj separately if its referenced from
      // inside a bigger string.
      if (obj.refs == 1) obj.refs = 2;
      return tag;
    });
  }

  exports.condense = function(sources, name) {
    if (typeof sources == "string") sources = [sources];
    if (!name) name = sources[0];

    var cx = tern.cx(), types = {}, haveType = false;
    var output = {"!name": name, "!types": types};
    var state = {sources: sources, tags: Object.create(null), cx: cx};

    for (var v in cx.topScope.props) {
      var typ = cx.topScope.props[v].getType();
      if (typ && sources.indexOf(typ.origin) > -1)
        output[v] = typ.getDesc(state);
    }

    for (var tag in state.tags) sanitize(state.tags[tag].structure, state);
    sanitize(output, state);

    for (var tag in state.tags) {
      var obj = state.tags[tag];
      if (obj.refs == 1) continue; // Inlined
      types[tag] = obj.structure;
      haveType = true;
    }
    if (!haveType) delete output["!types"];

    return output;
  };
})(typeof exports == "undefined" ? window.tern : exports);
