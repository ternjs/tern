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

  var flag_recGuard = 1, flag_initializer = 2, flag_definite = 4;

  // ABSTRACT VALUES

  function AVal(type) {
    this.types = [];
    // FIXME move inline with types
    this.scores = [];
    this.forward = [];
    this.flags = 0;
    if (type) type.propagate(this);
  }
  AVal.prototype = {
    addType: function(type) {
      // FIXME merge strategy -- [?] + [<int>] = [<int>], similar for object & fn types
      for (var i = 0; i < this.types.length; ++i) {
        if (this.types[i] == type) {
          ++this.scores[i];
          return;
        }
      }
      this.types.push(type);
      this.scores.push(1);
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

    hasType: function(type) {
      return this.types.indexOf(type) > -1;
    },
    isEmpty: function() { return this.types.length == 0; },

    typeHint: function(maxDepth) { return this.toString(maxDepth); },

    toString: function(maxDepth) {
      if (this.flags & flag_recGuard) return "<R>";
      this.flags |= flag_recGuard;

      var name = this.types.length ? this.summarizeType(maxDepth) : this.guessType(maxDepth);

      this.flags &= ~flag_recGuard;
      return name;
    },

    guessType: function(maxDepth) {
      if (this.hint) return this.hint.toString(maxDepth);

      var props = {}, propCount = 0, retval = "?";
      for (var i = 0; retval == "?" && i < this.forward.length; ++i) {
        var propagate = this.forward[i];
        if (propagate.typeHint) retval = propagate.typeHint();
        var prop = propagate.propHint && propagate.propHint();
        if (prop && !props[prop]) {
          props[propagate.prop] = true;
          ++propCount;
        }
      }
      if (retval == "?" && propCount) {
        var bestMatch, bestMatchTotalProps, bestScore = 0;
        for (var p in props) {
          var objs = cx.objProps[p];
          if (objs) for (var i = 0; i < objs.length; ++i) {
            var type = objs[i], score = 0, totalProps = 0;
            if (type == bestMatch) continue;
            for (var p2 in type.props) {
              if (p2 in props) ++score;
              ++totalProps;
            }
            if (score > bestScore || (score == score && totalProps < bestMatchTotalProps)) {
              bestScore = score;
              bestMatch = type;
              bestMatchTotalProps = totalProps;
            }
          }
        }
        if (bestMatch) {
          retval = bestMatch.toString(maxDepth);
        } else {
          retval = "{";
          for (var p in props) {
            if (retval.length != 1) retval += ", ";
            retval += p;
            if (retval.length > 20) break;
          }
          retval += "}";
        }
      }
      return retval;
    },

    summarizeType: function(maxDepth) {
      // FIXME this is a rather random heuristic
      var max = 0, found = [];
      for (var i = 0; i < this.types.length; ++i) {
        if (this.scores[i] > max) {
          max = this.scores[i];
          found.length = 0;
        }
        if (this.scores[i] == max) found.push(this.types[i]);
      }
      if (found.length > 3) return "?";
      for (var i = 0; i < found.length; ++i) found[i] = found[i].toString(maxDepth);
      found.sort();
      return found.join(" | ");
    }
  };

  // A variant of AVal used for unknown, dead-end values
  var ANull = {
    addType: function() {},
    propagate: function() {},
    getProp: function() { return ANull; },
    hasType: function() { return false; },
    isEmpty: function() { return true; },
    toString: function() { return "?"; }
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

  function IsCallee(self, args, retval) {
    this.self = self; this.args = args; this.retval = retval;
  }
  IsCallee.prototype = {
    addType: function(fn) {
      if (!(fn instanceof Fn)) return;
      if (this.args) for (var i = 0, e = Math.min(this.args.length, fn.args.length); i < e; ++i)
        this.args[i].propagate(fn.args[i]);
      if (this.self) this.self.propagate(fn.self);
      if (this.retval) {
        var rv = fn.computeRet && this.args && this.self ? fn.computeRet(this.self, this.args) : fn.retval;
        rv.propagate(this.retval);
      }
    },
    typeHint: function(maxDepth) {
      return new Fn(null, this.self, this.args, this.retval).toString(maxDepth);
    }
  };

  function IsProto(other) { this.other = other; }
  IsProto.getInstance = function(o) {
    if (!(o instanceof Obj)) return null;
    if (!o.instance) o.instance = new Obj(o);
    return o.instance;
  };
  IsProto.prototype = {
    addType: function(o) {
      var inst = IsProto.getInstance(o);
      if (inst) this.other.addType(inst);
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
    typeHint: function(maxDepth) {
      return this.other.toString(maxDepth);
    }
  };

  // TYPE OBJECTS

  function Type() {}
  Type.prototype = {
    propagate: function(c) { c.addType(this); },
    hasType: function(other) { return other == this; },
    isEmpty: function() { return false; },
    addType: function() {}
  };

  function Prim(proto, name) { this.name = name; this.proto = proto; }
  Prim.prototype = Object.create(Type.prototype);
  Prim.prototype.toString = function() { return this.name; };
  Prim.prototype.getProp = function(prop) {
    if (this.proto) return this.proto.getProp(prop);
    return cx.prim.undef;
  };

  function hop(obj, prop) {
    return Object.prototype.hasOwnProperty.call(obj, prop);
  }

  function Obj(proto, name) {
    if (proto && !name && proto.name) {
      var match = /^(.*)\.prototype$/.exec(proto.name);
      this.name = match ? match[1] : proto.name;
    } else {
      this.name = name;
    }
    if (proto === true) proto = cx.protos.Object;
    this.props = Object.create((proto && proto.props) || null);
  }
  Obj.prototype = Object.create(Type.prototype);
  Obj.prototype.toString = function(maxDepth) {
    if (!maxDepth && this.name) return this.name;
    // FIXME cache these strings?
    var props = [];
    for (var prop in this.props) if (prop != "<i>" && hop(this.props, prop)) {
      if (maxDepth)
        props.push(prop + ": " + this.props[prop].toString(maxDepth - 1));
      else if (this.props[prop].flags & flag_initializer)
        props.push(prop);
    }
    props.sort();
    return "{" + props.join(", ") + "}";
  };
  Obj.prototype.ensureProp = function(prop, alsoProto) {
    var found = this.props[prop];
    if (found && (alsoProto || hop(this.props, prop))) return found;
    var av = new AVal;
    if (!alsoProto) av.flags |= flag_definite;
    this.addProp(prop, av);
    return av;
  };
  Obj.prototype.getProp = function(prop) {
    return this.ensureProp(prop, true);
  };
  Obj.prototype.addProp = function(prop, val) {
    if (prop == "__proto__" || prop == "✖") return;
    this.props[prop] = val;
    // FIXME should objProps also list proto's props? maybe move back to objList?
    if (this.prev) return; // If this is a scope, it shouldn't be registered
    var found = cx.objProps[prop];
    if (found) found.push(this);
    else cx.objProps[prop] = [this];
  };

  Obj.fromInitializer = function(props, name) {
    var types = props.length && cx.objProps[props[0].name];
    // FIXME check prototype
    if (types) for (var i = 0; i < types.length; ++i) {
      var type = types[i], matching = 0;
      for (var p in type.props) {
        var prop = type.props[p];
        if ((prop.flags & flag_initializer) && props.some(function(x) {return x.name == p;}))
          ++matching;
      }
      if (matching == props.length) return type;
    }

    // Nothing found, allocate a new one
    var obj = new Obj(true, name);
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
      str += this.args[i].toString(maxDepth);
    }
    str += ")";
    var rettype = this.retval.toString(maxDepth);
    if (rettype != "?") str += " -> " + rettype;
    return str;
  };
  Fn.prototype.ensureProp = function(prop, alsoProto) {
    var newProto = this.name && prop == "prototype" && !hop(this.props, "prototype");
    var retval = Obj.prototype.ensureProp.call(this, prop, alsoProto && !newProto);
    if (newProto && retval.isEmpty() && alsoProto)
      retval.addType(new Obj(true, this.name + ".prototype"));
    return retval;
  };

  function Arr(contentType) {
    Obj.call(this, cx.protos.Array);
    var content = this.ensureProp("<i>");
    if (contentType) contentType.propagate(content);
  }
  Arr.prototype = Object.create(Obj.prototype);
  Arr.prototype.toString = function(maxDepth) {
    if (maxDepth) maxDepth--;
    return "[" + this.getProp("<i>").toString(maxDepth) + "]";
  };

  // INFERENCE CONTEXT

  var Context = exports.Context = function(environment) {
    this.objProps = Object.create(null);
    this.protos = Object.create(null);
    this.prim = Object.create(null);
    this.localProtos = null;

    exports.withContext(this, function() {
      cx.protos.Object = new Obj(null, "Object");
      cx.topScope = new Scope(null, true);
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

  function Scope(prev, proto) {
    Obj.call(this, proto, prev ? null : "global");
    this.prev = prev;
  }
  Scope.prototype = Object.create(Obj.prototype);
  Scope.prototype.lookup = function(name, force) {
    var self = this;
    for (;;) {
      if (name in self.props) return self.props[name];
      var prev = self.prev;
      if (prev) self = prev;
      else break;
    }
    if (force) return self.ensureProp(name);
  };

  // SCOPE GATHERING PASS

  var scopeGatherer = walk.make({
    Function: function(node, scope, c) {
      var inner = node.body.scope = new Scope(scope);
      var argVals = inner.argVals = [], argNames = inner.argNames = [];
      for (var i = 0; i < node.params.length; ++i) {
        var param = node.params[i];
        argNames.push(param);
        argVals.push(inner.ensureProp(param.name));
      }
      inner.retval = new AVal;
      inner.self = new AVal;
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

  function assignVar(name, scope, aval) {
    aval.propagate(scope.lookup(name, true));
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
      return IsProto.getInstance(cx.protos.RegExp);
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
      return Obj.fromInitializer(props, name);
    },
    FunctionExpression: function(node, scope, c, name) {
      var inner = node.body.scope;
      var aval = new AVal(new Fn(node.id ? node.id.name : name,
                                 inner.self, inner.argVals, inner.argNames, inner.retval));
      if (node.id)
        assignVar(node.id.name, node.body.scope, aval);
      c(node.body, scope, "ScopeBody");
      return aval;
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
        obj.propagate(new PropHasSubset(propName(node.left, scope, c), rhs));
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
      return this.CallExpression(node, scope, c, null, true);
    },
    CallExpression: function(node, scope, c, _name, isNew) {
      var callee, self, args = [];
      if (isNew) {
        callee = runInfer(node.callee, scope, c);
        self = new AVal;
        callee.getProp("prototype").propagate(new IsProto(self));
      } else if (node.callee.type == "MemberExpression") {
        self = runInfer(node.callee.object, scope, c);
        callee = self.getProp(propName(node.callee, scope, c));
      } else {
        callee = runInfer(node.callee, scope, c);
      }
      if (node.arguments) for (var i = 0; i < node.arguments.length; ++i)
        args.push(runInfer(node.arguments[i], scope, c));
      var retval = callee.retval || (callee.retval = new AVal);
      callee.propagate(new IsCallee(self, args, retval));
      return isNew ? self : retval;
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
      return scope.self || cx.prim.undef;
    },
    Literal: function(node, scope) {
      return literalType(node.value);
    }
  };

  function runInfer(node, scope, c, name) {
    return inferExprVisitor[node.type](node, scope, c, name);
  }

  var scopePasser = walk.make({
    ScopeBody: function(node, scope, c) { c(node, node.scope || scope); }
  });

  var inferWrapper = walk.make({
    Expression: runInfer,
    
    FunctionDeclaration: function(node, scope, c) {
      var inner = node.body.scope;
      var aval = new AVal(new Fn(node.id.name, inner.self, inner.argVals,
                                 inner.argNames, inner.retval));
      assignVar(node.id.name, scope, aval);
      c(node.body, scope, "ScopeBody");
    },

    VariableDeclaration: function(node, scope, c) {
      for (var i = 0; i < node.declarations.length; ++i) {
        var decl = node.declarations[i];
        if (decl.init) assignVar(decl.id.name, scope, runInfer(decl.init, scope, c, decl.id.name));
      }
    },

    ReturnStatement: function(node, scope, c) {
      if (node.argument)
        runInfer(node.argument, scope, c).propagate(scope.retval);
    }
  }, scopePasser);

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
    var found = cx.objProps[name];
    if (found) for (var i = 0; i < found.length; ++i) {
      var val = found[i].getProp(name);
      if (!val.isEmpty()) return val;
    }
    return ANull;
  }

  // FIXME too much duplication with runInfer above
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
      return Obj.fromInitializer(props);
    },
    FunctionExpression: function(node) {
      var inner = node.body.scope;
      return new Fn(node.id && node.id.name, inner.self, inner.argVals,
                    inner.argNames, inner.retval);
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
      return this.CallExpression(node, scope, true);
    },
    CallExpression: function(node, scope, isNew) {
      var callee, self, args = [];
      if (isNew) {
        callee = findType(node.callee, scope);
        self = new AVal;
        callee.getProp("prototype").propagate(new IsProto(self));
      } else if (node.callee.type == "MemberExpression") {
        self = findType(node.callee.object, scope);
        var propN = propName(node.callee, scope);
        callee = self.getProp(propN);
        if (callee.isEmpty()) callee = findByPropertyName(propN);
      } else {
        callee = findType(node.callee, scope);
      }
      if (node.arguments) for (var i = 0; i < node.arguments.length; ++i)
        args.push(findType(node.arguments[i], scope));
      if (isNew) return self;
      var retval = new AVal;
      callee.propagate(new IsCallee(self, args, retval));
      return retval;
    },
    MemberExpression: function(node, scope) {
      var propN = propName(node, scope);
      var prop = findType(node.object, scope).getProp(propN);
      return prop.isEmpty() ? findByPropertyName(propN) : prop;
    },
    Identifier: function(node, scope) {
      return scope.lookup(node.name, true);
    },
    ThisExpression: function(node, scope) {
      return scope.self || cx.prim.undef;
    },
    Literal: function(node) {
      return literalType(node.value);
    }
  };

  function findType(node, scope) {
    return typeFinder[node.type](node, scope);
  }

  exports.expressionType = function(ast, start, end) {
    var found = walk.findNodeAt(ast, start, end, "Expression", scopePasser, cx.topScope);
    if (!found) return null;
    return findType(found.node, found.state);
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
          retType = null;
          computeRet = this.parseRetType();
        } else retType = this.parseType();
      }
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
        if (word in cx.localProtos) return IsProto.getInstance(cx.localProtos[word]);
        if (word in cx.protos) return IsProto.getInstance(cx.protos[word]);
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
          if (lhs instanceof Fn) return lhs.retval; // FIXME this is a bit of a kludge
          var rv = new AVal;
          lhs.propagate(new IsCallee(null, null, rv));
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
