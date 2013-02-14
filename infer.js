(function(exports) {
  var acorn, walk;
  if (typeof require != "undefined") {
    acorn = require("acorn");
    acorn.parse_dammit = require("acorn/acorn_loose").parse_dammit;
    walk = require("acorn/util/walk");
  } else {
    acorn = window.acorn;
    walk = acorn.walk;
  }

  var toString = exports.toString = function(type, maxDepth) {
    return type ? type.toString(maxDepth) : "?";
  };

  // ABSTRACT VALUES

  function AVal(type) {
    this.types = [];
    this.forward = null;
    this.flags = 0;
    if (type) type.propagate(this);
  }
  AVal.prototype = {
    addType: function(type) {
      if (this.types.indexOf(type) > -1) return;
      this.types.push(type);
      if (this.forward) for (var i = 0; i < this.forward.length; ++i)
        this.forward[i].addType(type);
    },

    propagate: function(c) {
      (this.forward || (this.forward = [])).push(c);
      for (var i = 0; i < this.types.length; ++i)
        c.addType(this.types[i]);
    },

    getProp: function(prop) {
      var found = (this.props || (this.props = Object.create(null)))[prop];
      if (!found) {
        found = this.props[prop] = new AVal;
        this.propagate(new PropIsSubset(prop, found));
      }
      return found;
    },

    forAllProps: function(c) {
      this.propagate(new ForAllProps(c));
    },

    hasType: function(type) {
      return this.types.indexOf(type) > -1;
    },
    isEmpty: function() { return this.types.length == 0; },
    getFunctionType: function() {
      // FIXME find most complete one?
      for (var i = 0; i < this.types.length; ++i)
        if (this.types[i] instanceof Fn) return this.types[i];
    },

    getType: function() {
      if (this.types.length == 0) return this.makeupType();
      if (this.types.length == 1) return this.types[0];
      return canonicalType(this.types);
    },

    makeupType: function() {
      if (!this.forward) return null;
      for (var i = 0; i < this.forward.length; ++i) {
        var fw = this.forward[i], hint = fw.typeHint && fw.typeHint();
        if (hint && !hint.isEmpty()) return hint;
      }

      var props = Object.create(null), foundProp = null;
      for (var i = 0; i < this.forward.length; ++i) {
        var fw = this.forward[i], prop = fw.propHint && fw.propHint();
        if (prop && prop != "length" && prop != "<i>") {
          props[prop] = true;
          foundProp = prop;
        }
      }
      if (!foundProp) return null;

      var objs = objsWithProp(foundProp);
      if (objs) {
        var matches = [];
        search: for (var i = 0; i < objs.length; ++i) {
          var obj = objs[i];
          for (var prop in props) {
            var match = obj.props[prop];
            if (!match || !(match.flags & flag_definite)) continue search;
          }
          matches.push(obj);
        }
        return canonicalType(matches);
      }
    },

    typeHint: function() { return this.types.length ? this.getType() : null; },
    propagatesTo: function() { return this; },

    gatherProperties: function(prefix, direct, proto) {
      for (var i = 0; i < this.types.length; ++i)
        this.types[i].gatherProperties(prefix, direct, proto);
    }
  };

  function canonicalType(types) {
    var arrays = 0, fns = 0, objs = 0, prims = 0;
    for (var i = 0; i < types.length; ++i) {
      var tp = types[i];
      if (tp instanceof Arr) ++arrays;
      else if (tp instanceof Fn) ++fns;
      else if (tp instanceof Obj) ++objs;
      else if (tp instanceof Prim) ++prims;
    }
    var kinds = (arrays && 1) + (fns && 1) + (objs && 1) + (prims && 1);
    if (kinds > 1) return null;

    var maxScore = 0, maxTp = null;
    for (var i = 0; i < types.length; ++i) {
      var tp = types[i], score = 0;
      if (arrays) {
        score = tp.getProp("<i>").isEmpty() ? 1 : 2;
      } else if (fns) {
        score = 1;
        for (var j = 0; j < tp.args.length; ++j) if (!tp.args[j].isEmpty()) ++score;
        if (!tp.retval.isEmpty()) ++score;
      } else if (objs) {
        score = tp.name ? 100 : 1;
        // FIXME this heuristic is far-fetched.
        for (var prop in tp.props) if (hop(tp.props, prop) && tp.props[prop].flags & flag_definite) ++score;
        for (var o = tp; o; o = o.proto) if (o.provisionary) {
          score = 1;
          break;
        }
      } else if (prims) {
        score = 1;
      }
      if (score > maxScore) { maxScore = score; maxTp = tp; }
    }
    return maxTp;
  }

  // A variant of AVal used for unknown, dead-end values
  var ANull = {
    addType: function() {},
    propagate: function() {},
    getProp: function() { return ANull; },
    hasType: function() { return false; },
    isEmpty: function() { return true; },
    getFunctionType: function() {},
    getType: function() {},
    gatherProperties: function() {}
  };

  // PROPAGATION STRATEGIES

  function PropIsSubset(prop, target) {
    this.target = target; this.prop = prop;
  }
  PropIsSubset.prototype = {
    addType: function(type) {
      if (type.getProp)
        type.getProp(this.prop).propagate(this.target);
    },
    propHint: function() { return this.prop; },
    propagatesTo: function() {
      return {target: this.target, pathExt: "." + this.prop};
    }
  };

  function PropHasSubset(prop, target) {
    this.target = target; this.prop = prop;
  }
  PropHasSubset.prototype = {
    addType: function(type) {
      if (type.ensureProp)
        this.target.propagate(type.ensureProp(this.prop));
    },
    propHint: function() { return this.prop; }
  };

  function ForAllProps(c) { this.c = c; }
  ForAllProps.prototype.addType = function(type) {
    if (!(type instanceof Obj)) return;
    type.forAllProps(this.c);
  };

  function IsCallee(self, args, retval) {
    this.self = self; this.args = args; this.retval = retval;
  }
  IsCallee.prototype = {
    addType: function(fn) {
      if (!(fn instanceof Fn)) return;
      if (fn.computeRet) {
        fn.computeRet(this.self, this.args).propagate(this.retval);
      } else {
        for (var i = 0, e = Math.min(this.args.length, fn.args.length); i < e; ++i)
          this.args[i].propagate(fn.args[i]);
        this.self.propagate(fn.self);
        fn.retval.propagate(this.retval);
      }
    },
    typeHint: function() {
      var names = [];
      for (var i = 0; i < this.args.length; ++i) names.push("?");
      return new Fn(null, this.self, this.args, names, this.retval);
    }
  };

  function IfObjType(other) { this.other = other; }
  IfObjType.prototype.addType = function(obj) {
    if (obj instanceof Obj) this.other.addType(obj);
  };

  function HasMethodCall(propName, args, retval) {
    this.propName = propName; this.args = args; this.retval = retval;
  }
  HasMethodCall.prototype.addType = function(obj) {
    obj.getProp(this.propName).propagate(new IsCallee(obj, this.args, this.retval));
  };
  HasMethodCall.prototype.propHint = function() { return this.propName; };

  function IsCtor(target) { this.target = target; }
  IsCtor.prototype.addType = function(f) {
    if (!(f instanceof Fn)) return;
    f.getProp("prototype").propagate(new IsProto(f, this.target));
  };

  function IsProto(ctor, target) { this.ctor = ctor; this.target = target; }
  IsProto.prototype.addType = function(o) {
    if (!(o instanceof Obj)) return;

    if (!o.instances) o.instances = [];
    for (var i = 0; i < o.instances.length; ++i) {
      var cur = o.instances[i];
      if (cur.ctor == this.ctor) return this.target.addType(cur.instance);
    }
    var instance = new Obj(o);
    o.instances.push({ctor: this.ctor, instance: instance});
    this.target.addType(instance);
  };

  function IsAdded(other, target) {
    this.other = other; this.target = target;
  }
  IsAdded.prototype = {
    addType: function(type) {
      if (type == cx.str)
        this.target.addType(cx.str);
      else if (type == cx.num && this.other.hasType(cx.num))
        this.target.addType(cx.num);
    },
    typeHint: function() { return this.other; }
  };

  // TYPE OBJECTS

  function Type() {}
  Type.prototype = {
    propagate: function(c) { c.addType(this); },
    hasType: function(other) { return other == this; },
    isEmpty: function() { return false; },
    getFunctionType: function() {},
    getType: function() { return this; },
    addType: function() {},
    forAllProps: function() {}
  };

  function Prim(proto, name) { this.name = name; this.proto = proto; }
  exports.Prim = Prim;
  Prim.prototype = Object.create(Type.prototype);
  Prim.prototype.toString = function() { return this.name; };
  Prim.prototype.getProp = function(prop) {return this.proto.props[prop] || ANull;};
  Prim.prototype.gatherProperties = function(prefix, direct, proto) {
    if (this.proto) this.proto.gatherProperties(prefix, direct, proto);
  };

  function hop(obj, prop) {
    return Object.prototype.hasOwnProperty.call(obj, prop);
  }

  var flag_initializer = exports.flag_initializer = 1;
  var flag_definite = exports.flag_definite = 2;

  function Obj(proto, name, origin) {
    this.props = Object.create(null);
    this.proto = proto === true ? cx.protos.Object : proto;
    if (proto && !name && proto.name && !(this instanceof Fn)) {
      var match = /^(.*)\.prototype$/.exec(proto.name);
      this.name = match ? match[1] : proto.name;
    } else {
      this.name = name;
    }
    if (origin !== false) this.setOrigin(origin);

    if (this.proto && !this.prev) this.proto.forAllProps(this.onProtoProp.bind(this));
  }
  exports.Obj = Obj;
  Obj.prototype = Object.create(Type.prototype);
  Obj.prototype.toString = function(maxDepth) {
    if (!maxDepth && this.name) return this.name;
    var props = [];
    for (var prop in this.props) if (prop != "<i>" && this.props[prop].flags & flag_definite) {
      if (maxDepth)
        props.push(prop + ": " + toString(this.props[prop].getType(), maxDepth - 1));
      else if (this.props[prop].flags & flag_initializer)
        props.push(prop);
    }
    props.sort();
    return "{" + props.join(", ") + "}";
  };
  Obj.prototype.ensureProp = function(prop, alsoProto) {
    var found = this.props[prop];
    if (alsoProto) for (var p = this.proto; p && !found; p = p.proto) found = p.props[prop];
    if (found) {
      if (!alsoProto && !(found.flags & flag_definite)) {
        found.flags |= flag_definite;
        this.broadcastProp(prop, found, true);
      }
      return found;
    }

    var av = new AVal;
    if (prop == "__proto__" || prop == "✖") return av;

    this.props[prop] = av;
    if (!alsoProto) {
      av.flags |= flag_definite;
      this.broadcastProp(prop, av, true);
    }
    return av;
  };
  Obj.prototype.getProp = function(prop) {
    return this.ensureProp(prop, true);
  };
  Obj.prototype.broadcastProp = function(prop, val, local) {
    // If this is a scope, it shouldn't be registered
    if (local && !this.prev) registerProp(prop, this);

    if (this.onNewProp) for (var i = 0; i < this.onNewProp.length; ++i)
      this.onNewProp[i](prop, val, local);
  };
  Obj.prototype.onProtoProp = function(prop, val, local) {
    var val = this.props[prop];
    if (val) {
      if (val.flags & flag_definite) return;
      delete this.props[prop];
      this.proto.getProp(prop).propagate(val);
    } else {
      this.broadcastProp(prop, val, false);
    }
  };
  Obj.prototype.gatherProperties = function(prefix, direct, proto) {
    for (var prop in this.props) {
      if (prefix && prop.indexOf(prefix) != 0 || prop == "<i>") continue;
      var val = this.props[prop];
      if (!(val.flags & flag_definite)) continue;
      if (direct.indexOf(prop) > -1 || proto.indexOf(prop) > -1) continue;
      direct.push(prop);
    }
    if (this.proto) this.proto.gatherProperties(prefix, proto, proto);
  };
  Obj.prototype.forAllProps = function(c) {
    (this.onNewProp || (this.onNewProp = [])).push(c);
    for (var o = this; o; o = o.proto) {
      for (var prop in o.props) {
        var val = o.props[prop];
        if (val.flags & flag_definite) c(prop, val, o == this);
      }
    }
  };

  Obj.prototype.setOrigin = function(orig) {
    if (orig || (orig = cx.curOrigin)) this.origin = orig;
  };

  // FIXME this is too easily confused. Use types again (or give up on it entirely?)
  Obj.findByProps = function(props) {
    if (!props.length) return null;
    var types = objsWithProp(props[0].key.name);
    if (types) for (var i = 0; i < types.length; ++i) {
      var type = types[i], matching = 0;
      for (var p in type.props) {
        var prop = type.props[p];
        if ((prop.flags & flag_initializer) && props.some(function(x) {return x.key.name == p;}))
          ++matching;
      }
      if (matching == props.length) return type;
    }
  };

  function Fn(name, self, args, argNames, retval) {
    Obj.call(this, cx.protos.Function, name, false);
    this.self = self;
    this.args = args;
    this.argNames = argNames;
    this.retval = retval;
    this.setOrigin();
  }
  exports.Fn = Fn;
  Fn.prototype = Object.create(Obj.prototype);
  Fn.prototype.toString = function(maxDepth) {
    if (maxDepth) maxDepth--;
    var str = "fn(";
    for (var i = 0; i < this.args.length; ++i) {
      if (i) str += ", ";
      var name = this.argNames[i];
      if (name && typeof name != "string") name = name.name;
      if (name) str += name + ": ";
      str += toString(this.args[i].getType(), maxDepth);
    }
    str += ")";
    if (!this.retval.isEmpty())
      str += " -> " + toString(this.retval.getType(), maxDepth);
    return str;
  };
  Fn.prototype.ensureProp = function(prop, alsoProto) {
    var newProto = prop == "prototype" && !("prototype" in this.props);
    var retval = Obj.prototype.ensureProp.call(this, prop, alsoProto && !newProto);
    if (newProto) {
      var name = (this.name || "?") + ".prototype";
      retval.propagate({addType: function(t) {t.name = name;}});
      if (retval.isEmpty() && alsoProto) {
        var proto = new Obj(true);
        proto.provisionary = true;
        retval.addType(proto);
      }
    }
    return retval;
  };
  Fn.prototype.getFunctionType = function() { return this; };

  function Arr(contentType) {
    Obj.call(this, cx.protos.Array, false);
    var content = this.ensureProp("<i>");
    if (contentType) contentType.propagate(content);
  }
  exports.Arr = Arr;
  Arr.prototype = Object.create(Obj.prototype);
  Arr.prototype.toString = function(maxDepth) {
    if (maxDepth) maxDepth--;
    return "[" + toString(this.getProp("<i>").getType(), maxDepth) + "]";
  };

  // THE PROPERTY REGISTRY

  function registerProp(prop, obj) {
    var data = cx.props[prop] || (cx.props[prop] = []);
    data.push(obj);
  }

  function objsWithProp(prop) {
    return cx.props[prop];
  }

  // INFERENCE CONTEXT

  var Context = exports.Context = function(environment) {
    this.props = Object.create(null);
    this.protos = Object.create(null);
    this.prim = Object.create(null);
    this.origins = [];
    this.curOrigin = "ecma5";
    this.predefs = Object.create(null);
    this.shorthands = null;

    exports.withContext(this, function() {
      this.curOrigin = "ecma5";
      cx.protos.Object = new Obj(null, "Object.prototype");
      cx.topScope = new Scope();
      cx.protos.Array = new Obj(true, "Array.prototype");
      cx.protos.Function = new Obj(true, "Function.prototype");
      cx.protos.RegExp = new Obj(true, "RegExp.prototype");
      cx.protos.String = new Obj(true, "String.prototype");
      cx.protos.Number = new Obj(true, "Number.prototype");
      cx.protos.Boolean = new Obj(true, "Boolean.prototype");
      cx.str = new Prim(cx.protos.String, "string");
      cx.bool = new Prim(cx.protos.Boolean, "bool");
      cx.num = new Prim(cx.protos.Number, "number");
      this.curOrigin = null;

      if (environment) for (var i = 0; i < environment.length; ++i)
        loadEnvironment(environment[i]);
    });
  };

  var cx = null;
  exports.cx = function() { return cx; };

  exports.withContext = function(context, f) {
    var old = cx;
    cx = context || new Context("browser");
    try { return f(); }
    finally { cx = old; }
  };

  function addOrigin(origin) {
    if (cx.origins.indexOf(origin) < 0) cx.origins.push(origin);
  }

  // SCOPES

  function Scope(prev) {
    this.prev = prev;
    Obj.call(this, prev || true);
  }
  Scope.prototype = Object.create(Obj.prototype);
  Scope.prototype.getVar = function(name, define) {
    for (var s = this; ; s = s.proto) {
      var found = s.props[name];
      if (found) return found;
      if (s == cx.topScope) return s.ensureProp(name, !define);
    }
  };
  Scope.prototype.defVar = function(name) { return this.getVar(name, true); };
  Scope.prototype.findVar = function(name) {
    for (var s = this; s; s = s.proto) {
      var found = s.props[name];
      if (found) return found;
    }
  };

  function maybeTypeManipulator(scope, score) {
    if (!scope.typeManipScore) scope.typeManipScore = 0;
    scope.typeManipScore += score;
  }

  function maybeTagAsTypeManipulator(node, scope) {
    if (scope.typeManipScore && scope.typeManipScore / (node.end - node.start) > .01) {
      var computeRet = scope.fnType.computeRet = function(self, args) {
        // Prevent recursion
        this.computeRet = null;
        var scopeCopy = new Scope(scope.prev), fn = scope.fnType;
        for (var v in scope.props) {
          var local = scopeCopy.ensureProp(v);
          for (var i = 0; i < fn.argNames.length; ++i) if (fn.argNames[i].name == v && i < args.length)
            args[i].propagate(local);
        }
        scopeCopy.fnType = new Fn(fn.name, self, args, fn.argNames, new AVal);
        node.body.scope = scopeCopy;
        walk.recursive(node.body, scopeCopy, null, scopeGatherer);
        walk.recursive(node.body, scopeCopy, null, inferWrapper);
        this.computeRet = computeRet;
        return scopeCopy.fnType.retval;
      };
      return true;
    }
  }

  function maybeTagAsGeneric(node, fn) {
    if (!fn.retval.isEmpty()) return;

    function explore(aval, path, depth) {
      if (depth > 6 || !aval.forward) return;
      for (var i = 0; i < aval.forward.length; ++i) {
        var fw = aval.forward[i], prop = fw.propagatesTo && fw.propagatesTo();
        if (!prop) continue;
        var newPath = path, target;
        if (prop instanceof AVal) {
          target = prop;
        } else if (prop.target instanceof AVal) {
          newPath += prop.pathExt;
          target = prop.target;
        } else continue;
        if (target == fn.retval) throw {foundPath: newPath};
        explore(target, newPath, depth + 1);
      }
    }

    var foundPath;
    try {
      explore(fn.self, "$this", 0);
      for (var i = 0; i < fn.args.length; ++i)
        explore(fn.args[i], "$" + i, 0);
    } catch (e) {
      if (!(foundPath = e.foundPath)) throw e;
    }

    if (foundPath) {
      var p = new TypeParser(foundPath);
      fn.computeRet = p.parseRetType();
      fn.computeRetSource = foundPath;
      return true;
    }
  }

  // SCOPE GATHERING PASS

  var scopeGatherer = walk.make({
    Function: function(node, scope, c) {
      var inner = node.body.scope = new Scope(scope);
      var argVals = [], argNames = [];
      for (var i = 0; i < node.params.length; ++i) {
        var param = node.params[i];
        argNames.push(param);
        var val = inner.ensureProp(param.name);
        argVals.push(val);
        val.name = param;
      }
      inner.fnType = new Fn(node.id && node.id.name, new AVal, argVals, argNames, new AVal);
      inner.fnType.originNode = node;
      if (node.id) {
        var decl = node.type == "FunctionDeclaration";
        (decl ? scope : inner).ensureProp(node.id.name).name = node.id;
      }
      c(node.body, inner, "ScopeBody");
    },
    TryStatement: function(node, scope, c) {
      c(node.block, scope, "Statement");
      for (var i = 0; i < node.handlers.length; ++i) {
        var handler = node.handlers[i], name = handler.param.name;
        scope.ensureProp(name).name = handler.param;
        c(handler.body, scope, "ScopeBody");
      }
      if (node.finalizer) c(node.finalizer, scope, "Statement");
    },
    VariableDeclaration: function(node, scope, c) {
      for (var i = 0; i < node.declarations.length; ++i) {
        var decl = node.declarations[i];
        scope.ensureProp(decl.id.name).name = decl.id;
        if (decl.init) c(decl.init, scope, "Expression");
      }
    }
  });

  // CONSTRAINT GATHERING PASS

  function propName(node, scope, c) {
    var prop = node.property;
    if (!node.computed) return prop.name;
    if (prop.type == "Literal" && typeof prop.value == "string") return prop.value;
    if (c) infer(prop, scope, c, ANull);
    return "<i>";
  }

  function lvalName(node) {
    if (node.type == "Identifier") return node.name;
    if (node.type == "MemberExpression" && !node.computed) {
      if (node.object.type != "Identifier") return node.property.name;
      return node.object.name + "." + node.property.name;
    }
  }

  function getInstance(obj) {
    return obj.instance || (obj.instance = new Obj(obj));
  }

  function unopResultType(op) {
    switch (op) {
    case "+": case "-": case "~": return cx.num;
    case "!": return cx.bool;
    case "typeof": return cx.str;
    case "void": case "delete": return ANull;
    }
  }
  function binopIsBoolean(op) {
    switch (op) {
    case "==": case "!=": case "===": case "!==": case "<": case ">": case ">=": case "<=":
    case "in": case "instanceof": return true;
    }
  }
  function literalType(val) {
    switch (typeof val) {
    case "boolean": return cx.bool;
    case "number": return cx.num;
    case "string": return cx.str;
    case "object":
      if (!val) return ANull;
      return getInstance(cx.protos.RegExp);
    }
  }

  function join(a, b) {
    if (a == b) return a;
    var joined = new AVal;
    a.propagate(joined); b.propagate(joined);
    return joined;
  }

  function ret(f) {
    return function(node, scope, c, out, name) {
      var r = f(node, scope, c, name);
      if (out) r.propagate(out);
      return r;
    };
  }
  function fill(f) {
    return function(node, scope, c, out, name) {
      if (!out) out = new AVal;
      f(node, scope, c, out, name);
      return out;
    };
  }

  var inferExprVisitor = {
    ArrayExpression: ret(function(node, scope, c) {
      var eltval = new AVal;
      for (var i = 0; i < node.elements.length; ++i) {
        var elt = node.elements[i];
        if (elt) infer(elt, scope, c, eltval);
      }
      return new Arr(eltval);
    }),
    ObjectExpression: ret(function(node, scope, c, name) {
      var obj = Obj.findByProps(node.properties);
      if (!obj) {
        obj = new Obj(true, name);
        obj.originNode = node;
      }

      for (var i = 0; i < node.properties.length; ++i) {
        var prop = node.properties[i], val = obj.ensureProp(prop.key.name);
        val.flags |= flag_initializer;
        infer(prop.value, scope, c, val, prop.key.name);
      }
      return obj;
    }),
    FunctionExpression: ret(function(node, scope, c, name) {
      var inner = node.body.scope, fn = inner.fnType;
      if (name && !fn.name) fn.name = name;
      c(node.body, scope, "ScopeBody");
      maybeTagAsTypeManipulator(node, inner) || maybeTagAsGeneric(node, inner.fnType);
      if (node.id) inner.defVar(node.id.name).addType(fn);
      return fn;
    }),
    SequenceExpression: ret(function(node, scope, c) {
      for (var i = 0, l = node.expressions.length - 1; i < l; ++i)
        infer(node.expressions[i], scope, c, ANull);
      return infer(node.expressions[l], scope, c);
    }),
    UnaryExpression: ret(function(node, scope, c) {
      infer(node.argument, scope, c, ANull);
      return unopResultType(node.operator);
    }),
    UpdateExpression: ret(function(node, scope, c) {
      infer(node.argument, scope, c, ANull);
      return cx.num;
    }),
    BinaryExpression: ret(function(node, scope, c) {
      if (node.operator == "+") {
        var lhs = infer(node.left, scope, c);
        var rhs = infer(node.right, scope, c);
        if (lhs.hasType(cx.str) || rhs.hasType(cx.str)) return cx.str;
        if (lhs.hasType(cx.num) && rhs.hasType(cx.num)) return cx.num;
        var result = new AVal;
        lhs.propagate(new IsAdded(rhs, result));
        rhs.propagate(new IsAdded(lhs, result));
        return result;
      } else {
        infer(node.left, scope, c, ANull);
        infer(node.right, scope, c, ANull);
        return binopIsBoolean(node.operator) ? cx.bool : cx.num;
      }
    }),
    AssignmentExpression: ret(function(node, scope, c) {
      var rhs, name, pName;
      if (node.left.type == "MemberExpression") {
        pName = propName(node.left, scope, c);
        if (node.left.object.type == "Identifier")
          name = node.left.object.name + "." + pName;
      } else {
        name = node.left.name;
      }

      if (node.operator != "=" && node.operator != "+=") {
        infer(node.right, scope, c, ANull, name);
        rhs = cx.num;
      } else {
        rhs = infer(node.right, scope, c, null, name);
      }

      if (node.left.type == "MemberExpression") {
        var obj = infer(node.left.object, scope, c);
        if (pName == "prototype") maybeTypeManipulator(scope, 20);
        if (pName == "<i>") {
          // This is a hack to recognize for/in loops that copy
          // properties, and do the copying ourselves, insofar as we
          // manage, because such loops tend to be relevant for type
          // information.
          var v = node.left.property.name, local = scope.props[v], over = local && local.iteratesOver;
          if (over) {
            var fromRight = node.right.type == "MemberExpression" && node.right.computed && node.right.property.name == v;
            over.forAllProps(function(prop, val, local) {
              if (local && prop != "prototype" && prop != "<i>")
                obj.propagate(new PropHasSubset(prop, fromRight ? val : ANull));
            });
            return rhs;
          }
        }
        obj.propagate(new PropHasSubset(pName, rhs));
      } else { // Identifier
        rhs.propagate(scope.defVar(node.left.name));
      }
      return rhs;
    }),
    LogicalExpression: fill(function(node, scope, c, out) {
      infer(node.left, scope, c, out);
      infer(node.right, scope, c, out);
    }),
    ConditionalExpression: fill(function(node, scope, c, out) {
      infer(node.test, scope, c, ANull);
      infer(node.consequent, scope, c, out);
      infer(node.alternate, scope, c, out);
    }),
    NewExpression: fill(function(node, scope, c, out) {
      if (node.callee.type == "Identifier" && node.callee.name in scope.props)
        maybeTypeManipulator(scope, 20);

      for (var i = 0, args = []; i < node.arguments.length; ++i)
        args.push(infer(node.arguments[i], scope, c));
      var callee = infer(node.callee, scope, c);
      var self = new AVal;
      callee.propagate(new IsCtor(self));
      callee.propagate(new IsCallee(self, args, out));
      self.propagate(out);
    }),
    CallExpression: fill(function(node, scope, c, out) {
      for (var i = 0, args = []; i < node.arguments.length; ++i)
        args.push(infer(node.arguments[i], scope, c));
      if (node.callee.type == "MemberExpression") {
        var self = infer(node.callee.object, scope, c);
        self.propagate(new HasMethodCall(propName(node.callee, scope, c), args, out));
      } else {
        var callee = infer(node.callee, scope, c);
        callee.propagate(new IsCallee(ANull, args, out));
      }
    }),
    MemberExpression: ret(function(node, scope, c) {
      return infer(node.object, scope, c).getProp(propName(node, scope, c));
    }),
    Identifier: ret(function(node, scope) {
      if (node.name == "arguments" && !(node.name in scope.props))
        scope.ensureProp(node.name).addType(new Arr);
      return scope.getVar(node.name);
    }),
    ThisExpression: ret(function(node, scope) {
      return scope.fnType ? scope.fnType.self : ANull;
    }),
    Literal: ret(function(node, scope) {
      return literalType(node.value);
    })
  };

  function infer(node, scope, c, out, name) {
    return inferExprVisitor[node.type](node, scope, c, out, name);
  }

  var inferWrapper = walk.make({
    Expression: function(node, scope, c) {
      infer(node, scope, c, ANull);
    },
    
    FunctionDeclaration: function(node, scope, c) {
      var inner = node.body.scope, fn = inner.fnType;
      c(node.body, scope, "ScopeBody");
      maybeTagAsTypeManipulator(node, inner) || maybeTagAsGeneric(node, inner.fnType);
      scope.defVar(node.id.name).addType(fn);
    },

    VariableDeclaration: function(node, scope, c) {
      for (var i = 0; i < node.declarations.length; ++i) {
        var decl = node.declarations[i];
        if (decl.init)
          infer(decl.init, scope, c, scope.defVar(decl.id.name), decl.id.name);
      }
    },

    ReturnStatement: function(node, scope, c) {
      if (node.argument && scope.fnType)
        infer(node.argument, scope, c, scope.fnType.retval);
    },

    ForInStatement: function(node, scope, c) {
      var source = infer(node.right, scope, c);
      if ((node.right.type == "Identifier" && node.right.name in scope.props) ||
          (node.right.type == "MemberExpression" && node.right.property.name == "prototype")) {
        maybeTypeManipulator(scope, 5);
        var varName;
        if (node.left.type == "Identifier") {
          varName = node.left.name;
        } else if (node.left.type == "VariableDeclaration") {
          varName = node.left.declarations[0].id.name;
        }
        if (varName && varName in scope.props)
          scope.getVar(varName).iteratesOver = source;
      }
      c(node.body, scope, "Statement");
    },

    ScopeBody: function(node, scope, c) { c(node, node.scope || scope); }
  });

  exports.analyze = function(text, file, scope) {
    if (!file) file = "file#" + cx.origins.length;
    addOrigin(cx.curOrigin = file);

    var ast;
    try {
        ast = acorn.parse(text);
    } catch (e) {
      if (e instanceof SyntaxError) ast = acorn.parse_dammit(text);
      else throw e;
    }
    if (!scope) scope = cx.topScope;
    walk.recursive(ast, scope, null, scopeGatherer);
    walk.recursive(ast, scope, null, inferWrapper);
    cx.curOrigin = null;
    return {ast: ast, text: text, file: file};
  };

  // EXPRESSION TYPE DETERMINATION

  function findByPropertyName(name) {
    var found = objsWithProp(name);
    if (found) for (var i = 0; i < found.length; ++i) {
      var val = found[i].getProp(name);
      if (!val.isEmpty()) return val;
    }
    return ANull;
  }

  var typeFinder = {
    ArrayExpression: function(node, scope) {
      var eltval = new AVal;
      for (var i = 0; i < node.elements.length; ++i) {
        var elt = node.elements[i];
        if (elt) findType(elt, scope).propagate(eltval);
      }
      return new Arr(eltval);
    },
    ObjectExpression: function(node, scope) {
      if (node.properties.length) return Obj.findByProps(node.properties);
      else return new Obj(true);
    },
    FunctionExpression: function(node) {
      return node.body.scope.fnType;
    },
    SequenceExpression: function(node, scope) {
      return findType(node.expressions[node.expressions.length-1], scope);
    },
    UnaryExpression: function(node) {
      return unopResultType(node.operator);
    },
    UpdateExpression: function() {
      return cx.num;
    },
    BinaryExpression: function(node, scope) {
      if (binopIsBoolean(node.operator)) return cx.bool;
      if (node.operator == "+") {
        var lhs = findType(node.left, scope);
        var rhs = findType(node.right, scope);
        if (lhs.hasType(cx.str) || rhs.hasType(cx.str)) return cx.str;
      }
      return cx.num;
    },
    AssignmentExpression: function(node, scope) {
      return findType(node.right, scope);
    },
    LogicalExpression: function(node, scope) {
      var lhs = findType(node.left, scope);
      return lhs.isEmpty() ? findType(node.right, scope) : lhs;
    },
    ConditionalExpression: function(node, scope) {
      var lhs = findType(node.consequent, scope);
      return lhs.isEmpty() ? findType(node.alternate, scope) : lhs;
    },
    NewExpression: function(node, scope) {
      var f = findType(node.callee, scope).getFunctionType();
      var proto = f && f.getProp("prototype").getType();
      if (!proto) return ANull;
      if (proto.instances) return proto.instances[0].instance;
      else return proto;
    },
    CallExpression: function(node, scope) {
      var f = findType(node.callee, scope).getFunctionType();
      if (!f) return ANull;
      if (f.computeRet) {
        for (var i = 0, args = []; i < node.arguments.length; ++i)
          args.push(findType(node.arguments[i], scope));
        var self = ANull;
        if (node.callee.type == "MemberExpression")
          self = findType(node.callee.object, scope);
        return f.computeRet(self, args);
      } else {
        return f.retval;
      }
    },
    MemberExpression: function(node, scope) {
      var propN = propName(node, scope);
      var prop = findType(node.object, scope).getProp(propN);
      return prop.isEmpty() && propN != "<i>" ? findByPropertyName(propN) : prop;
    },
    Identifier: function(node, scope) {
      return scope.getVar(node.name, true);
    },
    ThisExpression: function(node, scope) {
      return scope.fnType ? scope.fnType.self : ANull;
    },
    Literal: function(node) {
      return literalType(node.value);
    }
  };

  function findType(node, scope) {
    var found = typeFinder[node.type](node, scope);
    if (found.isEmpty()) return found.getType() || found;
    return found;
  }

  var searchVisitor = walk.make({
    Function: function(node, st, c) {
      var scope = node.body.scope;
      if (node.id) c(node.id, scope);
      for (var i = 0; i < node.params.length; ++i)
        c(node.params[i], scope);
      c(node.body, scope, "ScopeBody");
    },
    TryStatement: function(node, st, c) {
      for (var i = 0; i < node.handlers.length; ++i)
        c(node.handlers[i].param, st, c);
      walk.base.TryStatement(node, st, c);
    },
    VariableDeclaration: function(node, st, c) {
      for (var i = 0; i < node.declarations.length; ++i) {
        var decl = node.declarations[i];
        c(decl.id, st);
        if (decl.init) c(decl.init, st, "Expression");
      }
    }
  });

  exports.findExpression = function(ast, start, end) {
    var test = function(_t, node) {return typeFinder.hasOwnProperty(node.type);};
    return walk.findNodeAt(ast, start, end, test, searchVisitor, cx.topScope);
  };

  exports.expressionType = function(found) {
    return findType(found.node, found.state);
  };

  exports.propertiesOf = function(type, prefix) {
    var direct = [], proto = [];
    type.gatherProperties(prefix, direct, proto);
    direct.sort();
    proto.sort();
    if (direct.length || proto.length || prefix.length < 2) return direct.concat(proto);
    for (var prop in cx.props) if (prop.indexOf(prefix) == 0) direct.push(prop);
    return direct;
  };

  // LOCAL-VARIABLE QUERIES

  var scopeAt = exports.scopeAt = function(ast, pos, defaultScope) {
    var found = walk.findNodeAround(ast, pos, "ScopeBody");
    if (found) return found.node.scope;
    else return defaultScope || cx.topScope;
  };

  exports.localsAt = function(ast, pos, prefix) {
    var scope = scopeAt(ast, pos), locals = [];
    scope.gatherProperties(prefix, locals, locals);
    locals.sort();
    return locals;
  };

  // CONTEXT POPULATING

  function TypeParser(spec) {
    this.pos = 0; this.spec = spec;
  }
  TypeParser.prototype = {
    eat: function(str) {
      if (str.length == 1 ? this.spec.charAt(this.pos) == str : this.spec.indexOf(str, this.pos) == this.pos) {
        this.pos += str.length;
        return true;
      }
    },
    word: function(re) {
      var word = "", ch, re = re || /[\w$]/;
      while ((ch = this.spec.charAt(this.pos)) && re.test(ch)) { word += ch; ++this.pos; }
      return word;
    },
    error: function() {
      throw new Error("Unrecognized type spec: " + this.spec + " " + this.pos);
    },
    parseFnType: function(name, top) {
      for (var args = [], names = [], i = 0; !this.eat(")"); ++i) {
        var colon = this.spec.indexOf(": ", this.pos), argname, aval;
        if (colon != -1) {
          argname = this.spec.slice(this.pos, colon);
          if (/^[$\w?]+$/.test(argname))
            this.pos = colon + 2;
          else
            argname = null;
        }
        names.push(argname);
        args.push(this.parseType());
        this.eat(", ");
      }
      var retType, computeRet;
      if (this.eat(" -> ")) {
        if (top && this.spec.indexOf("$", this.pos) > -1) {
          retType = ANull;
          computeRet = this.parseRetType();
        } else retType = this.parseType();
      } else retType = ANull;
      var fn = new Fn(name, ANull, args, names, retType);
      if (computeRet) fn.computeRet = computeRet;
      return fn;
    },
    parseType: function(name, top) {
      if (this.eat("fn(")) {
        return this.parseFnType(name, top);
      } else if (this.eat("[")) {
        var arr = new Arr(this.parseType());
        if (this.eat("]")) return arr;
      } else if (this.eat("+")) {
        var val = parsePath(this.word(/[<>\w$\/!]/));
        if (val instanceof Obj) return getInstance(val);
        else return val;
      } else if (this.eat("/")) {
        return parsePath("/" + this.word(/[<>\w$\/!]/));
      } else if (this.eat("?")) {
        return ANull;
      } else {
        var word = this.word(/[\w\.$]/);
        switch (word) {
        case "number": return cx.num;
        case "string": return cx.str;
        case "bool": return cx.bool;
        }
        if (word in cx.shorthands) return cx.shorthands[word];
        if (word in cx.protos) return getInstance(cx.protos[word]);
      }
      this.error();
    },
    parseRetTypeInner: function() {
      if (this.eat("[")) {
        var inner = this.parseRetType();
        this.eat("]") || this.error();
        return function(self, args) { return new Arr(inner(self, args)); };
      } else if (this.eat("$")) {
        var arg = this.word(/\d/);
        if (arg) {
          arg = Number(arg);
          return function(self, args) {return args[arg] || ANull;};
        } else if (this.eat("this")) {
          return function(self) {return self;};
        } else if (this.eat("Object_create")) {
          return function(self, args) {
            var result = new AVal;
            if (args[0]) args[0].propagate({addType: function(tp) {
              if (tp.isEmpty()) {
                result.addType(new Obj());
              } else if (tp instanceof Obj) {
                var derived = new Obj(tp), spec = args[1];
                if (spec instanceof AVal) spec = spec.types[0];
                if (spec instanceof Obj) for (var prop in spec.props) {
                  var cur = spec.props[prop].types[0];
                  var p = derived.ensureProp(prop);
                  if (cur && cur instanceof Obj && cur.props.value) {
                    var vtp = cur.props.value.getType();
                    if (vtp) p.addType(vtp);
                  }
                }
                result.addType(derived)
              }
            }});
            return result;
          };
        } else this.error();
      }
      var t = this.parseType();
      return function(){return t;};
    },
    parseRetType: function() {
      var inner = this.parseRetTypeInner();
      if (this.eat(".")) {
        var propName = this.word(/[\w<>$]/) || this.error();
        if (propName == "$ret") return function(self, args) {
          var lhs = inner(self, args);
          if (lhs.retval) return lhs.retval;
          var rv = new AVal;
          lhs.propagate(new IsCallee(ANull, [], rv));
          return rv;
        };
        return function(self, args) {return inner(self, args).getProp(propName);};
      } else return inner;
    }
  }

  function parseType(spec, name) {
    return new TypeParser(spec).parseType(name, true);
  }

  function parsePath(path) {
    var predef = cx.predefs[path];
    if (predef) return predef;

    var parts = path.split("/");
    var cur = cx.topScope;
    for (var i = 1; i < parts.length && cur != ANull; ++i) {
      var part = parts[i];
      if (!part) continue;
      if (part.charAt(0) == "!") {
        if (part == "!proto") {
          cur = (cur instanceof Obj && cur.proto) || ANull;
        } else {
          var fn = cur.getFunctionType();
          if (!fn) {
            cur = ANull;
          } else if (part == "!ret") {
            cur = fn.retval.getType() || ANull;
          } else {
            var arg = fn.args[Number(part.slice(1))];
            cur = (arg && arg.getType()) || ANull;
          }
        }
      } else {
        cur = cur.getProp(part).getType() || ANull;
      }
    }
    return cur;
  }

  function populate(obj, props, name) {
    for (var prop in props) if (hop(props, prop) && prop.charCodeAt(0) != 33) {
      var nm = name ? name + "." + prop : prop;
      var v = obj.ensureProp(prop);
      interpret(props[prop], nm).propagate(v);
    }
    return obj;
  }

  function interpret(spec, name) {
    if (typeof spec == "string") return parseType(spec, name);
    // Else, it is an object spec
    var obj;
    if (spec["!type"]) obj = interpret(spec["!type"], name);
    else if (spec["!stdProto"]) obj = cx.protos[spec["!stdProto"]];
    else if (spec["!isDef"]) obj = interpret(spec["!isDef"]);
    else obj = new Obj(spec["!proto"] ? interpret(spec["!proto"]) : true, name);
    return populate(obj, spec, name);
  }

  function pathToName(path) {
    var parts = path.split("/");
    var name = parts[parts.length - 1];
    if (!name || name.charAt(0) == "!") return null;
    var prev = parts[parts.length - 2];
    if (prev && prev.charAt(0) != "!") name = prev + "." + name;
    return name;
  }

  function loadEnvironment(data) {
    addOrigin(cx.curOrigin = data["!name"] || "env#" + cx.origins.length);

    var defs = data["!predef"];
    if (defs) for (var name in defs) if (hop(defs, name))
      cx.predefs[name] = interpret(defs[name], pathToName(name));

    cx.shorthands = Object.create(null);
    var shs = data["!shorthands"];
    if (shs) for (var name in shs) if (hop(shs, name))
      cx.shorthands[name] = interpret(shs[name]);

    populate(cx.topScope, data);

    var added = data["!added"];
    if (added) for (var path in added) {
      var lastSlash = path.lastIndexOf("/");
      if (lastSlash < 1) continue;
      var obj = parsePath(path.slice(0, lastSlash));
      interpret(added[path]).propagate(obj.getProp(path.slice(lastSlash + 1)));
    }

    cx.curOrigin = null;
  }

})(typeof exports == "undefined" ? window.tern || (window.tern = {}) : exports);
