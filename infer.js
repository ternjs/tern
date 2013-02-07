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
    this.forward = [];
    this.flags = 0;
    if (type) type.propagate(this);
  }
  AVal.prototype = {
    addType: function(type) {
      for (var i = 0; i < this.types.length; ++i) if (this.types[i] == type) return;
      this.types.push(type);
      for (var i = 0; i < this.forward.length; ++i)
        this.forward[i].addType(type);
    },

    propagate: function(c) {
      this.forward.push(c);
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

      var arrays = 0, fns = 0, objs = 0, prims = 0;
      for (var i = 0; i < this.types.length; ++i) {
        var tp = this.types[i];
        if (tp instanceof Arr) ++arrays;
        else if (tp instanceof Fn) ++fns;
        else if (tp instanceof Obj) ++objs;
        else if (tp instanceof Prim && tp != cx.prim.null && tp != cx.prim.undef) ++prims;
      }
      var kinds = (arrays && 1) + (fns && 1) + (objs && 1) + (prims && 1);
      if (kinds > 1) return null;

      var maxScore = 0, maxTp = null;
      for (var i = 0; i < this.types.length; ++i) {
        var tp = this.types[i], score = 0;
        if (arrays && tp instanceof Arr) {
          score = tp.getProp("<i>").isEmpty() ? 1 : 2;
        } else if (fns && tp instanceof Fn) {
          score = 1;
          for (var j = 0; j < tp.args.length; ++j) if (!tp.args[j].empty()) ++score;
          if (!tp.retval.isEmpty()) ++score;
        } else if (objs && tp instanceof Obj) {
          score = tp.name ? 100 : 1;
          for (var prop in tp.props) if (hop(tp.props, prop) && tp.props[prop].flags & flag_definite) ++score;
        } else if (prims && tp instanceof Prim && tp != cx.prim.undef && tp != cx.prim.null) {
          score = 1;
        }
        if (score > maxScore) { maxScore = score; maxTp = tp; }
      }
      return maxTp;
    },

    makeupType: function() {
      if (this.hint) return this.hint;

      for (var i = 0; i < this.forward.length; ++i) {
        var fw = this.forward[i], hint = fw.typeHint && fw.typeHint();
        if (hint && !hint.isEmpty()) return hint;
      }

      var props = Object.create(null), foundProp = null;
      for (var i = 0; i < this.forward.length; ++i) {
        var fw = this.forward[i], prop = fw.propHint && fw.propHint();
        if (prop) { props[prop] = true; foundProp = prop; }
      }
      if (!foundProp) return null;

      var objs = objsWithProp(foundProp);
      if (objs) search: for (var i = 0; i < objs.length; ++i) {
        var obj = objs[i];
        for (var prop in props) {
          var match = obj.props[prop];
          if (!match || !(match.flags & flag_definite)) continue search;
        }
        return obj;
      }
    },

    typeHint: function() { return this.types.length ? this.types[0] : null; },

    gatherProperties: function(prefix, direct, proto) {
      for (var i = 0; i < this.types.length; ++i)
        this.types[i].gatherProperties(prefix, direct, proto);
    }
  };

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
    propHint: function() { return this.prop; }
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

  function IsProto(other, ctor) { this.other = other; this.ctor = ctor; }
  IsProto.prototype = {
    addType: function(o) {
      if (!(o instanceof Obj)) return;
      if (!o.instances) o.instances = [];
      for (var i = 0; i < o.instances.length; ++i) {
        var cur = o.instances[i];
        if (cur.ctor == this.ctor) return this.other.addType(cur.instance);
      }
      var instance = new Obj(o);
      o.instances.push({ctor: this.ctor, instance: instance});
      this.other.addType(instance);
    }
  };

  function IsAdded(other, target) {
    this.other = other; this.target = target;
  }
  IsAdded.prototype = {
    addType: function(type) {
      if (type == cx.prim.str)
        this.target.addType(cx.prim.str);
      else if (type == cx.prim.num && this.other.hasType(cx.prim.num))
        this.target.addType(cx.prim.num);
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
  Prim.prototype = Object.create(Type.prototype);
  Prim.prototype.toString = function() { return this.name; };
  Prim.prototype.getProp = function(prop) {
    if (this.proto) return this.proto.getProp(prop);
    return cx.prim.undef;
  };
  Prim.prototype.gatherProperties = function(prefix, direct, proto) {
    if (this.proto) this.proto.gatherProperties(prefix, direct, proto);
  };

  function hop(obj, prop) {
    return Object.prototype.hasOwnProperty.call(obj, prop);
  }

  var flag_initializer = 1, flag_definite = 2;

  function Obj(proto, name) {
    if (proto && !name && proto.name) {
      var match = /^(.*)\.prototype$/.exec(proto.name);
      this.name = match ? match[1] : proto.name;
    } else {
      this.name = name;
    }
    if (proto === true) proto = cx.protos.Object;
    this.props = Object.create((proto && proto.props) || null);

    if (proto) proto.forAllProps(this.onProtoProp.bind(this));
  }
  Obj.prototype = Object.create(Type.prototype);
  Obj.prototype.toString = function(maxDepth) {
    if (!maxDepth && this.name) return this.name;
    var props = [];
    for (var prop in this.props) if (prop != "<i>" && hop(this.props, prop) && this.props[prop].flags & flag_definite) {
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
    if (found) {
      var own = hop(this.props, prop);
      if (own && !alsoProto && !(found.flags & flag_definite)) {
        found.flags |= flag_definite;
        this.broadcastProp(prop, found, true);
      }
      if (own || alsoProto) return found;
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
    if (hop(this.props, prop)) {
      var val = this.props[prop];
      if (val.flags & flag_definite) return;
      delete this.props[prop];
      this.props[prop].propagate(val);
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
      (hop(this.props, prop) ? direct : proto).push(prop);
    }
  };
  Obj.prototype.forAllProps = function(c) {
    (this.onNewProp || (this.onNewProp = [])).push(c);
    for (var prop in this.props) {
      var val = this.props[prop];
      if (val.flags & flag_definite) c(prop, val, hop(this.props, prop));
    }
  };

  Obj.fromInitializer = function(props, name, origin) {
    var types = props.length && objsWithProp(props[0].name);
    if (types) for (var i = 0; i < types.length; ++i) {
      var type = types[i], matching = 0;
      for (var p in type.props) if (hop(type.props, p)) {
        var prop = type.props[p];
        if ((prop.flags & flag_initializer) && props.some(function(x) {return x.name == p;}))
          ++matching;
      }
      if (matching == props.length) return type;
    }

    // Nothing found, allocate a new one
    var obj = new Obj(true, name);
    obj.origin = origin;
    for (var i = 0; i < props.length; ++i) {
      var prop = props[i], aval = obj.ensureProp(prop.name);
      aval.flags |= flag_initializer;
      prop.type.propagate(aval);
    }
    return obj;
  };

  function Fn(name, self, args, argNames, retval) {
    Obj.call(this, cx.protos.Function, name);
    this.self = self;
    this.args = args;
    this.argNames = argNames;
    this.retval = retval;
  }
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
    var newProto = this.name && prop == "prototype" && !hop(this.props, "prototype");
    var retval = Obj.prototype.ensureProp.call(this, prop, alsoProto && !newProto);
    if (newProto && retval.isEmpty() && alsoProto)
      retval.addType(new Obj(true, this.name + ".prototype"));
    return retval;
  };
  Fn.prototype.getFunctionType = function() { return this; };

  function Arr(contentType) {
    Obj.call(this, cx.protos.Array);
    var content = this.ensureProp("<i>");
    if (contentType) contentType.propagate(content);
  }
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
    this.localProtos = null;

    exports.withContext(this, function() {
      cx.protos.Object = new Obj(null, "Object");
      cx.topScope = new Scope();
      cx.protos.Array = new Obj(true, "Array.prototype");
      cx.protos.Function = new Obj(true, "Function.prototype");
      cx.protos.RegExp = new Obj(true, "RegExp.prototype");
      cx.protos.String = new Obj(true, "String.prototype");
      cx.protos.Number = new Obj(true, "Number.prototype");
      cx.protos.Boolean = new Obj(true, "Boolean.prototype");
      cx.prim.str = new Prim(cx.protos.String, "string");
      cx.prim.bool = new Prim(cx.protos.Boolean, "bool");
      cx.prim.num = new Prim(cx.protos.Number, "number");
      cx.prim.null = new Prim(null, "null");
      cx.prim.undef = new Prim(null, "undefined");

      if (environment) for (var i = 0; i < environment.length; ++i)
        loadEnvironment(environment[i]);
    });
  };

  var cx = null;

  exports.withContext = function(context, f) {
    var old = cx;
    cx = context || new Context("browser");
    try { return f(); }
    finally { cx = old; }
  };

  // SCOPES

  function Scope(prev) {
    Obj.call(this, prev || true);
    this.prev = prev;
  }
  Scope.prototype = Object.create(Obj.prototype);
  Scope.prototype.lookup = function(name, force) {
    if (name in this.props) return this.props[name];
    for (var top = this; top.prev; top = top.prev) {}
    if (force) return top.ensureProp(name, true);
  };

  function maybeTypeManipulator(scope, score) {
    if (!scope.typeManipScore) scope.typeManipScore = 0;
    scope.typeManipScore += score;
  }

  function maybeTagAsTypeManipulator(node, scope) {
    if (scope.typeManipScore && scope.typeManipScore / (node.end - node.start) > .01) {
      scope.fnType.computeRet = function(self, args) {
        var scopeCopy = new Scope(scope.prev), fn = scope.fnType;
        for (var v in scope.props) if (hop(scope.props, v)) {
          var local = scopeCopy.ensureProp(v);
          for (var i = 0; i < fn.argNames.length; ++i) if (fn.argNames[i].name == v && i < args.length)
            args[i].propagate(local);
        }
        scopeCopy.fnType = new Fn(fn.name, self, args, fn.argNames, new AVal);
        node.body.scope = scopeCopy;
        walk.recursive(node.body, scopeCopy, null, scopeGatherer);
        walk.recursive(node.body, scopeCopy, null, inferWrapper);
        return scopeCopy.fnType.retval;
      };
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
      inner.fnType.origin = node;
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

  function assignVar(name, scope, src) {
    src.propagate(scope.lookup(name, true));
  }

  function propName(node, scope, c) {
    var prop = node.property;
    if (!node.computed) return prop.name;
    if (prop.type == "Literal" && typeof prop.value == "string") return prop.value;
    if (c) runInfer(prop, scope, c);
    return "<i>";
  }

  function lvalName(node) {
    if (node.type == "Identifier") return node.name;
    if (node.type == "MemberExpression" && !node.computed) {
      if (node.object.type != "Identifier") return node.property.name;
      return node.object.name + "." + node.property.name;
    }
  }

  function setHint(aval, hint) {
    if (aval.isEmpty()) aval.hint = hint;
  }

  function getInstance(obj) {
    return obj.instance || (obj.instance = new Obj(obj));
  }

  function unopResultType(op) {
    switch (op) {
    case "+": case "-": case "~": return cx.prim.num;
    case "!": return cx.prim.bool;
    case "typeof": return cx.prim.str;
    case "void": case "delete": return cx.prim.undef;
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
    case "boolean": return cx.prim.bool;
    case "number": return cx.prim.num;
    case "string": return cx.prim.str;
    case "object":
      if (!val) return cx.prim.null;
      return getInstance(cx.protos.RegExp);
    }
  }

  function join(a, b) {
    if (a == b) return a;
    var joined = new AVal;
    a.propagate(joined); b.propagate(joined);
    return joined;
  }

  var inferExprVisitor = {
    ArrayExpression: function(node, scope, c) {
      var eltval = new AVal;
      for (var i = 0; i < node.elements.length; ++i) {
        var elt = node.elements[i];
        if (elt) runInfer(elt, scope, c).propagate(eltval);
      }
      return new Arr(eltval);
    },
    ObjectExpression: function(node, scope, c, name) {
      var props = [];
      for (var i = 0; i < node.properties.length; ++i) {
        var p = node.properties[i];
        props.push({name: p.key.name, type: runInfer(p.value, scope, c)});
      }
      return Obj.fromInitializer(props, name, node);
    },
    FunctionExpression: function(node, scope, c, name) {
      c(node.body, scope, "ScopeBody");
      var inner = node.body.scope, fn = inner.fnType;
      if (name && !fn.name) fn.name = name;
      if (node.id)
        assignVar(node.id.name, node.body.scope, fn);
      maybeTagAsTypeManipulator(node, inner);
      return fn;
    },
    SequenceExpression: function(node, scope, c) {
      var v;
      for (var i = 0; i < node.expressions.length; ++i)
        v = runInfer(node.expressions[i], scope, c);
      return v;
    },
    UnaryExpression: function(node, scope, c) {
      runInfer(node.argument, scope, c);
      return unopResultType(node.operator);
    },
    UpdateExpression: function(node, scope, c) {
      runInfer(node.argument, scope, c);
      return cx.prim.num;
    },
    BinaryExpression: function(node, scope, c) {
      var lhs = runInfer(node.left, scope, c);
      var rhs = runInfer(node.right, scope, c);
      if (node.operator == "+") {
        if (lhs.hasType(cx.prim.str) || rhs.hasType(cx.prim.str)) return cx.prim.str;
        if (lhs.hasType(cx.prim.num) && rhs.hasType(cx.prim.num)) return cx.prim.num;
        var result = new AVal;
        lhs.propagate(new IsAdded(rhs, result));
        rhs.propagate(new IsAdded(lhs, result));
        return result;
      }
      var isBool = binopIsBoolean(node.operator);
      if (!isBool) { setHint(lhs, cx.prim.num); setHint(rhs, cx.prim.num); }
      return isBool ? cx.prim.bool : cx.prim.num;
    },
    AssignmentExpression: function(node, scope, c) {
      var rhs = runInfer(node.right, scope, c, lvalName(node.left));
      if (node.operator != "=" && node.operator != "+=")
        rhs = cx.prim.num;

      if (node.left.type == "MemberExpression") {
        var obj = runInfer(node.left.object, scope, c);
        var pName = propName(node.left, scope, c);
        obj.propagate(new PropHasSubset(pName, rhs));
        if (pName == "prototype") maybeTypeManipulator(scope, 20);
        if (pName == "<i>" && node.right.type == "MemberExpression" && node.right.computed) {
          // This is a hack to recognize for/in loops that copy
          // properties, and do the copying ourselves, insofar as we
          // manage, because such loops tend to be relevant for type
          // information.
          var over = scope.lookup(node.right.property.name).iteratesOver;
          if (over) over.forAllProps(function(prop, val, local) {
            if (local && prop != "prototype" && prop != "<i>")
              obj.propagate(new PropHasSubset(prop, val));
          });
        }
      } else { // Identifier
        assignVar(node.left.name, scope, rhs);
      }
      return rhs;
    },
    LogicalExpression: function(node, scope, c) {
      return join(runInfer(node.left, scope, c),
                  runInfer(node.right, scope, c));
    },
    ConditionalExpression: function(node, scope, c) {
      runInfer(node.test, scope, c);
      return join(runInfer(node.consequent, scope, c),
                  runInfer(node.alternate, scope, c));
    },
    NewExpression: function(node, scope, c) {
      if (node.callee.type == "Identifier" && hop(scope.props, node.callee.name))
        maybeTypeManipulator(scope, 20);

      for (var i = 0, args = []; i < node.arguments.length; ++i)
        args.push(runInfer(node.arguments[i], scope, c));
      var callee = runInfer(node.callee, scope, c);
      var self = new AVal;
      callee.getProp("prototype").propagate(new IsProto(self, callee));
      var retval = callee.retval || (callee.retval = new AVal);
      callee.propagate(new IsCallee(self, args, retval));
      retval.propagate(new IfObjType(self));
      return self;
    },
    CallExpression: function(node, scope, c, _name) {
      for (var i = 0, args = []; i < node.arguments.length; ++i)
        args.push(runInfer(node.arguments[i], scope, c));
      if (node.callee.type == "MemberExpression") {
        var self = runInfer(node.callee.object, scope, c);
        var retval = new AVal;
        self.propagate(new HasMethodCall(propName(node.callee, scope, c), args, retval));
        return retval;
      } else {
        var callee = runInfer(node.callee, scope, c);
        var retval = callee.retval || (callee.retval = new AVal);
        callee.propagate(new IsCallee(ANull, args, retval));
        return retval;
      }
    },
    MemberExpression: function(node, scope, c) {
      return runInfer(node.object, scope, c).getProp(propName(node, scope, c));
    },
    Identifier: function(node, scope) {
      if (node.name == "arguments" && !scope.props.arguments)
        scope.ensureProp(node.name).addType(new Arr);
      return scope.lookup(node.name, true);
    },
    ThisExpression: function(node, scope, c) {
      return scope.fnType ? scope.fnType.self : cx.prim.undef;
    },
    Literal: function(node, scope) {
      return literalType(node.value);
    }
  };

  function runInfer(node, scope, c, name) {
    return inferExprVisitor[node.type](node, scope, c, name);
  }

  var inferWrapper = walk.make({
    Expression: runInfer,
    
    FunctionDeclaration: function(node, scope, c) {
      var inner = node.body.scope, fn = inner.fnType;
      assignVar(node.id.name, scope, fn);
      maybeTagAsTypeManipulator(node, inner);
      c(node.body, scope, "ScopeBody");
    },

    VariableDeclaration: function(node, scope, c) {
      for (var i = 0; i < node.declarations.length; ++i) {
        var decl = node.declarations[i];
        if (decl.init) assignVar(decl.id.name, scope, runInfer(decl.init, scope, c, decl.id.name));
      }
    },

    ReturnStatement: function(node, scope, c) {
      if (node.argument && scope.fnType)
        runInfer(node.argument, scope, c).propagate(scope.fnType.retval);
    },

    ForInStatement: function(node, scope, c) {
      var source = runInfer(node.right, scope, c);
      if ((node.right.type == "Identifier" && hop(scope.props, node.right.name)) ||
          (node.right.type == "MemberExpression" && node.right.property.name == "prototype")) {
        maybeTypeManipulator(scope, 5);
        var varName;
        if (node.left.type == "Identifier") {
          varName = node.left.name;
        } else if (node.left.type == "VariableDeclaration") {
          varName = node.left.declarations[0].id.name;
        }
        if (varName && hop(scope.props, varName))
          scope.lookup(varName).iteratesOver = source;
      }
      c(node.body, scope, "Statement");
    },

    ScopeBody: function(node, scope, c) { c(node, node.scope || scope); }
  });

  exports.analyze = function(text, file) {
    var ast;
    try {
        ast = acorn.parse(text);
    } catch (e) {
      if (e instanceof SyntaxError) ast = acorn.parse_dammit(text);
      else throw e;
    }
    walk.recursive(ast, cx.topScope, null, scopeGatherer);
    walk.recursive(ast, cx.topScope, null, inferWrapper);
    return {ast: ast, text: text, scope: cx.topScope, file: file};
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
      var props = [];
      for (var i = 0; i < node.properties.length; ++i) {
        var p = node.properties[i];
        props.push({name: p.key.name, type: findType(p.value, scope)});
      }
      return Obj.fromInitializer(props, null, node);
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
      return cx.prim.num;
    },
    BinaryExpression: function(node, scope) {
      if (binopIsBoolean(node.operator)) return cx.prim.bool;
      if (node.operator == "+") {
        var lhs = findType(node.left, scope);
        var rhs = findType(node.right, scope);
        if (lhs.hasType(cx.prim.str) || rhs.hasType(cx.prim.str)) return cx.prim.str;
      }
      return cx.prim.num;
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
          self = findType(node.callee.object);
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
      return scope.lookup(node.name, true);
    },
    ThisExpression: function(node, scope) {
      return scope.fnType ? scope.fnType.self : cx.prim.undef;
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
    var found = walk.findNodeAt(ast, start, end, null, searchVisitor, cx.topScope);
    if (!found || !typeFinder.hasOwnProperty(found.node.type)) return null;
    return found;
  };

  exports.expressionType = function(found) {
    return findType(found.node, found.state);
  };

  exports.propertiesOf = function(type, prefix) {
    var direct = [], proto = [];
    type.gatherProperties(prefix, direct, proto);
    direct.sort();
    proto.sort();
    return direct.concat(proto);
  };

  // LOCAL-VARIABLE QUERIES

  exports.localsAt = function(ast, pos, prefix) {
    var found = walk.findNodeAround(ast, pos, "ScopeBody");
    var scope = found ? found.node.scope : cx.topScope, locals = [];
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
      } else {
        var word = this.word(/[<>\w$?]/);
        switch (word) {
        case "number": return cx.prim.num;
        case "string": return cx.prim.str;
        case "bool": return cx.prim.bool;
        case "null": return cx.prim.null;
        case "undefined": return cx.prim.undef;
        case "?": return ANull;
        case "<top>": return cx.topScope;
        }
        if (word in cx.localProtos) return getInstance(cx.localProtos[word]);
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
        if (arg) { arg = Number(arg); return function(self, args) {return args[arg];}; }
        if (this.eat("this")) return function(self) {return self;};
        if (this.eat("Object_create")) return function(self, args) {
          var result = new AVal;
          (args[0] || cx.prim.null).propagate({addType: function(tp) {
            if (tp == cx.prim.null) {
              result.addType(new Obj());
            } else if (tp instanceof Obj) {
              var derived = new Obj(tp), spec = args[1];
              if (spec instanceof AVal) spec = spec.types[0];
              if (spec instanceof Obj) for (var prop in spec.props) if (hop(spec.props, prop)) {
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
        else this.error();
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
    else if (spec["!isProto"]) obj = cx.localProtos[spec["!isProto"]];
    else obj = new Obj(spec["!proto"] ? interpret(spec["!proto"]) : true, name);
    return populate(obj, spec, name);
  }

  function loadEnvironment(data) {
    cx.localProtos = Object.create(null);
    var ps = data["!types"];
    if (ps) for (var name in ps) if (hop(ps, name))
      cx.localProtos[name] = interpret(ps[name], name + ".prototype");
    populate(cx.topScope, data);
  }

})(typeof exports == "undefined" ? (window.tern = {}) : exports);
