// Main type inference engine

// Walks an AST, building up a graph of abstract values and contraints
// that cause types to flow from one node to another. Also defines a
// number of utilities for accessing ASTs and scopes.

// Analysis is done in a context, which is tracked by the dynamically
// bound cx variable. Use withContext to set the current context.

// For memory-saving reasons, individual types export an interface
// similar to abstract values (which can hold multiple types), and can
// thus be used in place abstract values that only ever contain a
// single type.

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    return mod(exports, require("acorn/acorn"), require("acorn/acorn_loose"), require("acorn/util/walk"),
               require("./env"), require("./jsdoc"));
  if (typeof define == "function" && define.amd) // AMD
    return define(["exports", "acorn/acorn", "acorn/acorn_loose", "acorn/util/walk", "./env", "./jsdoc"], mod);
  mod(self.tern || (self.tern = {}), acorn, acorn, acorn.walk, tern.env, tern.jsdoc); // Plain browser env
})(function(exports, acorn, acorn_loose, walk, env, jsdoc) {
  "use strict";

  // Delayed initialization because of cyclic dependencies.
  env = exports.env = env.init({}, exports);
  jsdoc = exports.jsdoc = jsdoc.init({}, exports);

  var toString = exports.toString = function(type, maxDepth, parent) {
    return !type || type == parent ? "?": type.toString(maxDepth);
  };

  // ABSTRACT VALUES

  var AVal = exports.AVal = function(type) {
    this.types = [];
    this.forward = null;
    if (type) type.propagate(this);
  };
  AVal.prototype = {
    addType: function(type) {
      if (this.types.indexOf(type) > -1) return;

      this.types.push(type);
      if (this.forward) for (var i = 0, maxLen = this.forward.length + 10;
                             i < this.forward.length && i < maxLen; ++i)
        this.forward[i].addType(type);
    },

    propagate: function(c) {
      if (c == ANull || (c instanceof Type)) return;
      (this.forward || (this.forward = [])).push(c);
      for (var i = 0, maxLen = this.types.length + 10;
           i < this.types.length && i < maxLen; ++i)
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
      for (var i = this.types.length - 1; i >= 0; --i)
        if (this.types[i] instanceof Fn) return this.types[i];
    },

    getType: function(guess) {
      if (this.types.length == 0 && guess !== false) return this.makeupType();
      if (this.types.length == 1) return this.types[0];
      return canonicalType(this.types);
    },

    makeupType: function() {
      if (!this.forward) return null;
      for (var i = this.forward.length - 1; i >= 0; --i) {
        var fw = this.forward[i], hint = fw.typeHint && fw.typeHint();
        if (hint && !hint.isEmpty()) {guessing = true; return hint;}
      }

      var props = Object.create(null), foundProp = null;
      for (var i = 0; i < this.forward.length; ++i) {
        var fw = this.forward[i], prop = fw.propHint && fw.propHint();
        if (prop && prop != "length" && prop != "<i>" && prop != "✖") {
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
          for (var prop in props) if (!obj.hasProp(prop)) continue search;
          matches.push(obj);
        }
        var canon = canonicalType(matches);
        if (canon) {guessing = true; return canon;}
      }
    },

    typeHint: function() { return this.types.length ? this.getType() : null; },
    propagatesTo: function() { return this; },

    gatherProperties: function(f, depth) {
      for (var i = 0; i < this.types.length; ++i)
        this.types[i].gatherProperties(f, depth);
    }
  };

  function canonicalType(types) {
    var arrays = 0, fns = 0, objs = 0, prim = null;
    for (var i = 0; i < types.length; ++i) {
      var tp = types[i];
      if (tp instanceof Arr) ++arrays;
      else if (tp instanceof Fn) ++fns;
      else if (tp instanceof Obj) ++objs;
      else if (tp instanceof Prim) {
        if (prim && tp.name != prim.name) return null;
        prim = tp;
      }
    }
    var kinds = (arrays && 1) + (fns && 1) + (objs && 1) + (prim && 1);
    if (kinds > 1) return null;
    if (prim) return prim;

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
        score = tp.name ? 100 : 2;
        for (var o = tp; o; o = o.proto) if (o.provisionary) {
          score = 1;
          break;
        }
      } else if (prims) {
        score = 1;
      }
      if (score >= maxScore) { maxScore = score; maxTp = tp; }
    }
    return maxTp;
  }

  // A variant of AVal used for unknown, dead-end values
  var ANull = exports.ANull = {
    addType: function() {},
    propagate: function() {},
    getProp: function() { return ANull; },
    forAllProps: function() {},
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

  var PropHasSubset = exports.PropHasSubset = function(prop, target) {
    this.target = target; this.prop = prop;
  }
  PropHasSubset.prototype = {
    addType: function(type) {
      if (type.defProp)
        this.target.propagate(type.defProp(this.prop));
    },
    propHint: function() { return this.prop; }
  };

  function ForAllProps(c) { this.c = c; }
  ForAllProps.prototype.addType = function(type) {
    if (!(type instanceof Obj)) return;
    type.forAllProps(this.c);
  };

  var IsCallee = exports.IsCallee = function(self, args, argNodes, retval) {
    this.self = self; this.args = args; this.argNodes = argNodes; this.retval = retval || ANull;
  }
  IsCallee.prototype = {
    addType: function(fn) {
      if (!(fn instanceof Fn)) return;
      for (var i = 0, e = Math.min(this.args.length, fn.args.length); i < e; ++i)
        this.args[i].propagate(fn.args[i]);
      this.self.propagate(fn.self);
      if (fn.computeRet)
        fn.computeRet(this.self, this.args, this.argNodes).propagate(this.retval);
      else
        fn.retval.propagate(this.retval);
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

  function HasMethodCall(propName, args, argNodes, retval) {
    this.propName = propName; this.args = args; this.argNodes = argNodes; this.retval = retval;
  }
  HasMethodCall.prototype.addType = function(obj) {
    obj.getProp(this.propName).propagate(new IsCallee(obj, this.args, this.argNodes, this.retval));
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

  var Type = exports.Type = function() {};
  Type.prototype = {
    propagate: function(c) { c.addType(this); },
    hasType: function(other) { return other == this; },
    isEmpty: function() { return false; },
    typeHint: function() { return this; },
    getFunctionType: function() {},
    getType: function() { return this; },
    addType: function() {},
    forAllProps: function() {}
  };

  var Prim = exports.Prim = function(proto, name) { this.name = name; this.proto = proto; };
  Prim.prototype = Object.create(Type.prototype);
  Prim.prototype.toString = function() { return this.name; };
  Prim.prototype.getProp = function(prop) {return this.proto.props[prop] || ANull;};
  Prim.prototype.gatherProperties = function(f, depth) {
    if (this.proto) this.proto.gatherProperties(f, depth);
  };

  var Obj = exports.Obj = function(proto, name, origin) {
    if (!this.props) this.props = Object.create(null);
    this.proto = proto === true ? cx.protos.Object : proto;
    if (proto && !name && proto.name && !(this instanceof Fn)) {
      var match = /^(.*)\.prototype$/.exec(this.proto.name);
      this.name = match ? match[1] : proto.name;
    } else {
      this.name = name;
    }
    this.maybeProps = null;
    if (origin !== false) this.setOrigin(origin);


    return this;
  };
  Obj.prototype = Object.create(Type.prototype);
  Obj.prototype.toString = function(maxDepth) {
    if (!maxDepth && this.name) return this.name;
    var props = [];
    for (var prop in this.props) if (prop != "<i>") {
      if (maxDepth)
        props.push(prop + ": " + toString(this.props[prop].getType(), maxDepth - 1));
      else if (this.props[prop].initializer)
        props.push(prop);
    }
    props.sort();
    return "{" + props.join(", ") + "}";
  };
  Obj.prototype.hasProp = function(prop, searchProto) {
    var found = this.props[prop];
    if (searchProto !== false)
      for (var p = this.proto; p && !found; p = p.proto) found = p.props[prop];
    return found;
  },
  Obj.prototype.defProp = function(prop) {
    var found = this.hasProp(prop, false);
    if (found) return found;
    if (prop == "__proto__" || prop == "✖") return new AVal;

    var av = this.maybeProps && this.maybeProps[prop];
    if (av) {
      delete this.maybeProps[prop];
      this.maybeUnregProtoPropHandler();
    } else {
      av = new AVal;
    }

    this.props[prop] = av;
    this.broadcastProp(prop, av, true);
    return av;
  };
  Obj.prototype.getProp = function(prop) {
    var found = this.hasProp(prop, true) || (this.maybeProps && this.maybeProps[prop]);
    if (found) return found;
    if (!this.maybeProps) {
      this.maybeProps = Object.create(null);
      if (this.proto) this.proto.forAllProps(this);
    }
    return this.maybeProps[prop] = new AVal;
  };
  Obj.prototype.broadcastProp = function(prop, val, local) {
    // If this is a scope, it shouldn't be registered
    if (local && !this.prev) registerProp(prop, this);

    if (this.onNewProp) for (var i = 0; i < this.onNewProp.length; ++i) {
      var h = this.onNewProp[i];
      h.onProtoProp ? h.onProtoProp(prop, val, local) : h(prop, val, local);
    }
  };
  Obj.prototype.onProtoProp = function(prop, val, local) {
    var maybe = this.maybeProps && this.maybeProps[prop];
    if (maybe) {
      delete this.maybeProps[prop];
      this.maybeUnregProtoPropHandler();
      this.proto.getProp(prop).propagate(maybe);
    }
    this.broadcastProp(prop, val, false);
  };
  Obj.prototype.forAllProps = function(c) {
    if (!this.onNewProp) {
      this.onNewProp = [];
      if (this.proto) this.proto.forAllProps(this);
    }
    this.onNewProp.push(c);
    for (var o = this; o; o = o.proto) for (var prop in o.props) {
      if (c.onProtoProp)
        c.onProtoProp(prop, o.props[prop], o == this);
      else
        c(prop, o.props[prop], o == this);
    }
  };
  Obj.prototype.maybeUnregProtoPropHandler = function() {
    if (this.maybeProps) {
      for (var _n in this.maybeProps) return;
      this.maybeProps = null;
    }
    if (!this.proto || this.onNewProp && this.onNewProp.length) return;
    this.proto.unregPropHandler(this);
  };
  Obj.prototype.unregPropHandler = function(handler) {
    for (var i = 0; i < this.onNewProp.length; ++i)
      if (this.onNewProp[i] == handler) { this.onNewProp.splice(i, 1); break; }
    this.maybeUnregProtoPropHandler();
  };

  Obj.prototype.gatherProperties = function(f, depth) {
    for (var prop in this.props) if (prop != "<i>")
      f(prop, this, depth);
    if (this.proto) this.proto.gatherProperties(f, depth + 1);
  };

  Obj.prototype.setOrigin = function(orig) {
    if (orig || (orig = cx.curOrigin)) this.origin = orig;
  };

  // FIXME this is too easily confused. Use types again (or give up on it entirely?)
  Obj.findByProps = function(props) {
    if (!props.length) return null;
    var types = objsWithProp(props[0].key.name);
    if (types) outer: for (var i = 0; i < types.length; ++i) {
      var type = types[i], matching = 0;
      for (var p in type.props) {
        var prop = type.props[p];
        if (prop.initializer) {
          if (!props.some(function(x) {return x.key.name == p;})) continue outer;
          ++matching;
        }
      }
      if (matching == props.length) return type;
    }
  };

  var Fn = exports.Fn = function(name, self, args, argNames, retval) {
    Obj.call(this, cx.protos.Function, name, false);
    this.self = self;
    this.args = args;
    this.argNames = argNames;
    this.retval = retval;
    this.setOrigin();
    return this;
  };
  Fn.prototype = Object.create(Obj.prototype);
  Fn.prototype.toString = function(maxDepth) {
    if (maxDepth) maxDepth--;
    var str = "fn(";
    for (var i = 0; i < this.args.length; ++i) {
      if (i) str += ", ";
      var name = this.argNames[i];
      if (name && name != "?") str += name + ": ";
      str += toString(this.args[i].getType(), maxDepth, this);
    }
    str += ")";
    if (!this.retval.isEmpty())
      str += " -> " + toString(this.retval.getType(), maxDepth, this);
    return str;
  };
  Fn.prototype.getProp = function(prop) {
    if (prop == "prototype") {
      var known = this.hasProp(prop);
      if (!known) {
        known = this.defProp(prop);
        if (known.isEmpty()) {
          if (this.name) {
            var name = this.name + ".prototype";
            known.propagate({addType: function(t) {if (!t.name) t.name = name;}});
          }
          var proto = new Obj(true);
          proto.provisionary = true;
          known.addType(proto);
        }
      }
      return known;
    }
    return Obj.prototype.getProp.call(this, prop);
  };
  Fn.prototype.getFunctionType = function() { return this; };

  var Arr = exports.Arr = function(contentType) {
    Obj.call(this, cx.protos.Array, false);
    var content = this.defProp("<i>");
    if (contentType) contentType.propagate(content);
    return this;
  };
  Arr.prototype = Object.create(Obj.prototype);
  Arr.prototype.toString = function(maxDepth) {
    return "[" + toString(this.getProp("<i>").getType(), maxDepth, this) + "]";
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

  var Context = exports.Context = function(environment, parent) {
    this.parent = parent;
    this.props = Object.create(null);
    this.protos = Object.create(null);
    this.prim = Object.create(null);
    this.origins = [];
    this.curOrigin = "ecma5";
    this.paths = Object.create(null);

    exports.withContext(this, function() {
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
      cx.curOrigin = null;

      if (environment) for (var i = 0; i < environment.length; ++i)
        env.loadEnvironment(environment[i]);
    });
  };

  var cx = null;
  exports.cx = function() { return cx; };

  exports.withContext = function(context, f) {
    var old = cx;
    cx = context;
    try { return f(); }
    finally { cx = old; }
  };

  exports.addOrigin = function(origin) {
    if (cx.origins.indexOf(origin) < 0) cx.origins.push(origin);
  };

  // SCOPES

  var Scope = exports.Scope = function(prev) {
    this.prev = prev;
    Obj.call(this, prev || true);
  };
  Scope.prototype = Object.create(Obj.prototype);
  Scope.prototype.defVar = function(name) {
    for (var s = this; ; s = s.proto) {
      var found = s.props[name];
      if (found) return found;
      if (!s.prev) return s.defProp(name);
    }
  };

  function maybeTypeManipulator(scope, score) {
    if (!scope.typeManipScore) scope.typeManipScore = 0;
    scope.typeManipScore += score;
  }

  function maybeTagAsTypeManipulator(node, scope) {
    if (scope.typeManipScore && scope.typeManipScore / (node.end - node.start) > .01) {
      var fn = scope.fnType;
      // Disconnect the arg avals, so that we can add info to them without side effects
      for (var i = 0; i < fn.args.length; ++i) fn.args[i] = new AVal;
      fn.self = new AVal;
      var computeRet = fn.computeRet = function(self, args) {
        // Prevent recursion
        this.computeRet = null;
        var scopeCopy = new Scope(scope.prev);
        for (var v in scope.props) {
          var local = scopeCopy.defProp(v);
          for (var i = 0; i < fn.argNames.length; ++i) if (fn.argNames[i] == v && i < args.length)
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
    var target = fn.retval, targetInner, asArray;
    if (!target.isEmpty() && (targetInner = target.getType()) instanceof Arr)
      target = asArray = targetInner.getProp("<i>");
    if (!target.isEmpty()) return;

    function explore(aval, path, depth) {
      if (depth > 3 || !aval.forward) return;
      for (var i = 0; i < aval.forward.length; ++i) {
        var fw = aval.forward[i], prop = fw.propagatesTo && fw.propagatesTo();
        if (!prop) continue;
        var newPath = path, dest;
        if (prop instanceof AVal) {
          dest = prop;
        } else if (prop.target instanceof AVal) {
          newPath += prop.pathExt;
          dest = prop.target;
        } else continue;
        if (dest == target) throw {foundPath: newPath};
        explore(dest, newPath, depth + 1);
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
      if (asArray) foundPath = "[" + foundPath + "]";
      var p = new env.TypeParser(foundPath);
      fn.computeRet = p.parseRetType();
      fn.computeRetSource = foundPath;
      return true;
    }
  }

  // SCOPE GATHERING PASS

  function addVar(scope, name) {
    var val = scope.defProp(name.name);
    val.name = name;
    val.origin = cx.curOrigin;
    return val;
  }

  var scopeGatherer = walk.make({
    Function: function(node, scope, c) {
      var inner = node.body.scope = new Scope(scope);
      var argVals = [], argNames = [];
      for (var i = 0; i < node.params.length; ++i) {
        var param = node.params[i];
        argNames.push(param.name);
        argVals.push(addVar(inner, param));
      }
      inner.fnType = new Fn(node.id && node.id.name, new AVal, argVals, argNames, new AVal);
      inner.fnType.originNode = node;
      if (node.id) {
        var decl = node.type == "FunctionDeclaration";
        addVar(decl ? scope : inner, node.id);
      }
      c(node.body, inner, "ScopeBody");
    },
    TryStatement: function(node, scope, c) {
      c(node.block, scope, "Statement");
      for (var i = 0; i < node.handlers.length; ++i) {
        var handler = node.handlers[i], name = handler.param.name;
        addVar(scope, handler.param);
        c(handler.body, scope, "ScopeBody");
      }
      if (node.finalizer) c(node.finalizer, scope, "Statement");
    },
    VariableDeclaration: function(node, scope, c) {
      for (var i = 0; i < node.declarations.length; ++i) {
        var decl = node.declarations[i];
        addVar(scope, decl.id);
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

  exports.getInstance = function(obj) {
    return obj.instance || (obj.instance = new Obj(obj));
  };

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
      return exports.getInstance(cx.protos.RegExp);
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
        var prop = node.properties[i], val = obj.defProp(prop.key.name);
        val.initializer = true;
        infer(prop.value, scope, c, val, prop.key.name);
      }
      return obj;
    }),
    FunctionExpression: ret(function(node, scope, c, name) {
      var inner = node.body.scope, fn = inner.fnType;
      if (name && !fn.name) fn.name = name;
      c(node.body, scope, "ScopeBody");
      maybeTagAsTypeManipulator(node, inner) || maybeTagAsGeneric(node, inner.fnType);
      if (node.id) inner.getProp(node.id.name).addType(fn);
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
      callee.propagate(new IsCallee(self, args, node.arguments, out));
      self.propagate(out);
    }),
    CallExpression: fill(function(node, scope, c, out) {
      for (var i = 0, args = []; i < node.arguments.length; ++i)
        args.push(infer(node.arguments[i], scope, c));
      if (node.callee.type == "MemberExpression") {
        var self = infer(node.callee.object, scope, c);
        self.propagate(new HasMethodCall(propName(node.callee, scope, c), args, node.arguments, out));
      } else {
        var callee = infer(node.callee, scope, c);
        callee.propagate(new IsCallee(cx.topScope, args, node.arguments, out));
      }
    }),
    MemberExpression: ret(function(node, scope, c) {
      return infer(node.object, scope, c).getProp(propName(node, scope, c));
    }),
    Identifier: ret(function(node, scope) {
      if (node.name == "arguments" && !(node.name in scope.props))
        scope.defProp(node.name).addType(new Arr);
      return scope.getProp(node.name);
    }),
    ThisExpression: ret(function(node, scope) {
      return scope.fnType ? scope.fnType.self : cx.topScope;
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
      scope.getProp(node.id.name).addType(fn);
    },

    VariableDeclaration: function(node, scope, c) {
      for (var i = 0; i < node.declarations.length; ++i) {
        var decl = node.declarations[i];
        if (decl.init)
          infer(decl.init, scope, c, scope.getProp(decl.id.name), decl.id.name);
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
          scope.getProp(varName).iteratesOver = source;
      }
      c(node.body, scope, "Statement");
    },

    ScopeBody: function(node, scope, c) { c(node, node.scope || scope); }
  });

  exports.analyze = function(text, file, scope) {
    if (!file) file = "file#" + cx.origins.length;
    exports.addOrigin(cx.curOrigin = file);

    var jsDoc = [], options = {onComment: jsdoc.gather(jsDoc)}, ast;
    try { ast = acorn.parse(text, options); }
    catch(e) {
      jsDoc.length = 0;
      ast = acorn_loose.parse_dammit(text, options);
    }

    if (!scope) scope = cx.topScope;
    walk.recursive(ast, scope, null, scopeGatherer);
    walk.recursive(ast, scope, null, inferWrapper);
    for (var i = 0; i < jsDoc.length; ++i)
      jsdoc.applyType(jsDoc[i], ast, scope, walk);
    cx.curOrigin = null;
    return {ast: ast, text: text, file: file};
  };

  // EXPRESSION TYPE DETERMINATION

  function findByPropertyName(name) {
    guessing = true;
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
        return f.computeRet(self, args, node.arguments);
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
      return scope.hasProp(node.name) || ANull;
    },
    ThisExpression: function(node, scope) {
      return scope.fnType ? scope.fnType.self : cx.topScope;
    },
    Literal: function(node) {
      return literalType(node.value);
    }
  };

  function findType(node, scope) {
    var found = typeFinder[node.type](node, scope);
    if (found.isEmpty()) found = found.getType() || found;
    return found;
  }

  var searchVisitor = exports.searchVisitor = walk.make({
    Function: function(node, st, c) {
      var scope = node.body.scope;
      if (node.id) c(node.id, scope);
      for (var i = 0; i < node.params.length; ++i)
        c(node.params[i], scope);
      c(node.body, scope, "ScopeBody");
    },
    TryStatement: function(node, st, c) {
      for (var i = 0; i < node.handlers.length; ++i)
        c(node.handlers[i].param, st);
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

  exports.findExpressionAt = function(ast, start, end, defaultScope, filter) {
    var test = filter || function(_t, node) {return typeFinder.hasOwnProperty(node.type);};
    return walk.findNodeAt(ast, start, end, test, searchVisitor, defaultScope || cx.topScope);
  };

  exports.findExpressionAround = function(ast, start, end, defaultScope, filter) {
    var test = filter || function(_t, node) {
      if (start != null && node.start > start) return false;
      return typeFinder.hasOwnProperty(node.type);
    };
    return walk.findNodeAround(ast, end, test, searchVisitor, defaultScope || cx.topScope);
  };

  exports.expressionType = function(found) {
    return findType(found.node, found.state);
  };

  // Flag used to indicate that some wild guessing was used to produce
  // a type or set of completions.
  var guessing = false;

  exports.resetGuessing = function(val) { guessing = val; };
  exports.didGuess = function() { return guessing; };

  exports.forAllPropertiesOf = function(type, f) {
    type.gatherProperties(f, 0);
  };

  var refFindWalker = walk.make({}, searchVisitor);

  exports.findRefs = function(ast, name, scope, f) {
    refFindWalker.Identifier = function(node, sc) {
      if (node.name != name) return;
      for (; sc; sc = sc.prev) {
        if (sc == scope) f(node);
        if (name in sc.props) return;
      }
    };
    walk.recursive(ast, cx.topScope, null, refFindWalker);
  };

  // LOCAL-VARIABLE QUERIES

  var scopeAt = exports.scopeAt = function(ast, pos, defaultScope) {
    var found = walk.findNodeAround(ast, pos, "ScopeBody");
    if (found) return found.node.scope;
    else return defaultScope || cx.topScope;
  };

  exports.forAllLocalsAt = function(ast, pos, defaultScope, f) {
    var scope = scopeAt(ast, pos, defaultScope), locals = [];
    scope.gatherProperties(f, 0);
  };
});
