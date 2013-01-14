var acorn = require("acorn"), walk = require("acorn/util/walk");
var fs = require("fs");

var flag_recGuard = 1, flag_speculative = 2, flag_initializer = 4;

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

  dominantType: function() {
    var max = 0, maxType;
    for (var i = 0; i < this.types.length; ++i) if (this.scores[i] > max) {
      max = this.scores[i];
      maxType = this.types[i];
    }
    return maxType;
  },

  isDominant: function(type) {
    var max = 0, typeScore = 0;
    for (var i = 0; i < this.types.length; ++i) {
      if (this.types[i] == type) typeScore = this.scores[i];
      if (this.scores[i] > max) max = this.scores[i];
    }
    return max == typeScore;
  },

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

    var props = {}, propCount = 0, retval = "<?>";
    for (var i = 0; retval == "<?>" && i < this.forward.length; ++i) {
      var propagate = this.forward[i];
      if (propagate.typeHint) retval = propagate.typeHint();
      var prop = propagate.propHint && propagate.propHint();
      if (prop && !props[prop]) {
        props[propagate.prop] = true;
        ++propCount;
      }
    }
    if (retval == "<?>" && propCount) {
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
    if (found.length > 3) return "<?>";
    for (var i = 0; i < found.length; ++i) found[i] = found[i].toString(maxDepth);
    found.sort();
    return found.join(" | ");
  }
};

// PROPAGATION STRATEGIES

function PropIsSubset(prop, target) {
  this.target = target; this.prop = prop;
}
PropIsSubset.prototype = {
  addType: function(type) {
    if (type.ensureProp)
      type.ensureProp(this.prop, true).propagate(this.target);
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
    for (var i = 0, e = Math.min(this.args.length, fn.args.length); i < e; ++i)
      this.args[i].propagate(fn.args[i]);
    if (this.self) this.self.propagate(fn.self);
    fn.retval.propagate(this.retval);
  },
  typeHint: function(maxDepth) {
    return new Fn(null, this.self, this.args, this.retval).toString(maxDepth);
  }
};

function IsProto(name) { this.name = name; }
IsProto.prototype = {
  addType: function(o) {
    if (o instanceof Obj && !o.name) o.name = this.name;
  },
  typeHint: function() { return this.name; }
};

function hasType(type, set) {
  return set == type || set.types && set.types.indexOf(type) > -1;
}

function IsAdded(other, target) {
  this.other = other; this.target = target;
}
IsAdded.prototype = {
  addType: function(type) {
    if (type == _str)
      this.target.addType(_str);
    else if (type == _num && hasType(_num, this.other))
      this.target.addType(_num);
  },
  typeHint: function(maxDepth) {
    return this.other.toString(maxDepth);
  }
};

// TYPE OBJECTS

// FIXME handle reads from prototypes of primitive types (str.slice)
// somehow

function Prim(name) { this.name = name; }
Prim.prototype = {
  toString: function() { return this.name; },
  propagate: function(c) { c.addType(this); },
  sameType: function(other) { return other == this; }
};

var _num = new Prim("<number>");
var _str = new Prim("<string>");
var _bool = new Prim("<bool>");
var _null = new Prim("<null>");
var _undef = new Prim("<undefined>");

function Obj(name) {
  this.props = Object.create(null);
  this.name = name;
  cx.objTypes.push(this);
}
Obj.prototype = {
  toString: function(maxDepth) {
    if (!maxDepth && this.name) return this.name;
    // FIXME cache these strings?
    var props = [];
    for (var prop in this.props) {
      if (maxDepth)
        props.push(prop + ": " + this.props[prop].toString(maxDepth - 1));
      else if (this.props[prop].flags & flag_initializer)
        props.push(prop);
    }
    props.sort();
    return "{" + props.join(", ") + "}";
  },
  ensureProp: function(prop, speculative) {
    var found = this.props[prop];
    if (found) {
      if (!speculative && found.flags & flag_speculative)
        found.flags &= ~flag_speculative;
      return found;
    }
    var av = new AVal;
    if (speculative) av.flags |= flag_speculative;
    this.addProp(prop, av);
    return av;
  },
  addProp: function(prop, val) {
    this.props[prop] = val;
    var found = cx.objProps[prop];
    if (found) found.push(this);
    else cx.objProps[prop] = [this];
  },
  propagate: function(c) { c.addType(this); }
};

Obj.fromInitializer = function(props, name) {
  var types = props.length && cx.objProps[props[0].name];
  if (types) for (var i = 0; i < types.length; ++i) {
    var type = types[i], matching = 0;
    for (var p in type.props) {
      var prop = type.props[p];
      if ((prop.flags & flag_initializer) && props.some(function(x) {
        return x.name == p && compatible(prop, x.type);
      }))
        ++matching;
    }
    if (matching == props.length) return type;
  }

  // Nothing found, allocate a new one
  var obj = new Obj(name);
  for (var i = 0; i < props.length; ++i) {
    var prop = props[i], aval = obj.ensureProp(prop.name);
    aval.flags |= flag_initializer;
    prop.type.propagate(aval);
  }
  return obj;
};

function compatible(one, two) {
  if (two instanceof AVal) {
    return !one.types.length || !two.types.length || one.isDominant(two.dominantType());
  } else {
    return one.isDominant(two);
  }
}

function Fn(name, self, args, retval) {
  Obj.call(this, name);
  this.self = self;
  this.args = args;
  this.retval = retval;
}
Fn.prototype = Object.create(Obj.prototype);
Fn.prototype.toString = function(maxDepth) {
  if (maxDepth) maxDepth--;
  var str = "fn(" + this.args.map(function(x) { return x.toString(maxDepth); }).join(", ") + ")";
  var rettype = this.retval.toString(maxDepth);
  if (rettype != "<?>") str += " -> " + rettype;
  return str;
};
Fn.prototype.ensureProp = function(prop, speculative) {
  var newProto = this.name && prop == "prototype" && !("prototype" in this.props);
  var retval = Obj.prototype.ensureProp.call(this, prop, speculative);
  if (newProto) retval.propagate(new IsProto(this.name));
  return retval;
};

// INFERENCE CONTEXT

function Context() {
  this.objTypes = [];
  this.objProps = Object.create(null);
  this.topScope = new Scope();
}

var cx = null;

exports.withContext = function(context, f) {
  var old = cx;
  cx = context || new Context();
  try { return f(); }
  finally { cx = old; }
};

/*var Object_prototype = new Obj(null, props({
  toString: Fn([], _str),
  valueOf: Fn([], _num),
  hasOwnProperty: Fn([_str], _bool),
  propertyIsEnumerable: Fn([_str], _bool)
}));*/

// SCOPES

function Var(node) {
  this.node = node;
  this.aval = new AVal;
}
// FIXME bind arguments
function Scope(prev) {
  this.vars = Object.create(null);
  this.prev = prev;
}

function lookup(name, scope) {
  for (; scope; scope = scope.prev) {
    var found = scope.vars[name];
    if (found) return found;
  }
}
function topScope(scope) {
  while (scope.prev) scope = scope.prev;
  return scope;
}

// SCOPE GATHERING PASS

var scopeGatherer = walk.make({
  Function: function(node, scope, c) {
    var inner = node.body.scope = new Scope(scope), argvals = inner.argvals = [];
    for (var i = 0; i < node.params.length; ++i) {
      var param = node.params[i], v = new Var(param);
      inner.vars[param.name] = v;
      argvals.push(v.aval);
    }
    inner.retval = new AVal;
    inner.self = new AVal;
    if (node.id) {
      var decl = node.type == "FunctionDeclaration";
      (decl ? scope : inner).vars[node.id.name] = new Var(node.id);
    }
    c(node.body, inner, "ScopeBody");
  },
  TryStatement: function(node, scope, c) {
    c(node.block, scope, "Statement");
    for (var i = 0; i < node.handlers.length; ++i) {
      var handler = node.handlers[i], name = handler.param.name;
      if (!(name in scope.vars)) scope.vars[name] = new Var(handler.param);
      c(handler.body, scope, "ScopeBody");
    }
    if (node.finalizer) c(node.finalizer, scope, "Statement");
  },
  VariableDeclaration: function(node, scope, c) {
    for (var i = 0; i < node.declarations.length; ++i) {
      var decl = node.declarations[i];
      if (!(decl.id.name in scope.vars))
        scope.vars[decl.id.name] = new Var(decl.id);
      if (decl.init) c(decl.init, scope, "Expression");
    }
  }
});

function assignVar(name, scope, aval) {
  var found = lookup(name, scope);
  if (!found) found = topScope(scope).vars[name] = new Var();
  aval.propagate(found.aval);
}

function propName(node, scope, c) {
  var prop = node.property;
  if (!node.computed) return prop.name;
  if (prop.type == "Literal" && typeof prop.value == "string") return prop.value;
  runInfer(prop, scope, c);
  return "<i>";
}
function getProp(obj, propName) {
  var pn = "prop_" + propName, found = obj[pn];
  if (!found) {
    found = obj[pn] = new AVal;
    obj.propagate(new PropIsSubset(propName, found));
  }
  return found;
}

function lvalName(node) {
  if (node.type == "Identifier") return node.name;
  if (node.type == "MemberExpression" && node.computed) return node.property.name;
}

function setHint(aval, hint) {
  if (aval instanceof AVal && !aval.types.length) aval.hint = hint;
}

function unopResultType(op) {
  switch (op) {
  case "+": case "-": case "~": return _num;
  case "!": return _bool;
  case "typeof": return _str;
  case "void": case "delete": return _undef;
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
  case "boolean": return _bool;
  case "number": return _num;
  case "string": return _str;
  case "object":
    if (!val) return _null;
    // FIXME return something for regexps
    return new Obj();
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
    // FIXME implement array type
    var arr = new Obj();
    eltval.propagate(arr.ensureProp("<i>"));
    return arr;
  },
  ObjectExpression: function(node, scope, c, name) {
    var props = [];
    for (var i = 0; i < node.properties.length; ++i) {
      var p = node.properties[i];
      props.push({name: p.key.name, type: runInfer(p.value, scope, c)});
    }
    // FIXME better heuristic for whether this is the top scope (i.e. module top scope)
    return Obj.fromInitializer(props, !scope.prev && name);
  },
  FunctionExpression: function(node, scope, c, name) {
    var inner = node.body.scope;
    var aval = new AVal(new Fn(node.id ? node.id.name : name,
                               inner.self, inner.argvals, inner.retval));
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
    return _num;
  },
  BinaryExpression: function(node, scope, c) {
    var lhs = runInfer(node.left, scope, c);
    var rhs = runInfer(node.right, scope, c);
    if (node.operator == "+") {
      if (hasType(_str, lhs) || hasType(_str, rhs)) return _str;
      if (hasType(_num, lhs) && hasType(_num, rhs)) return _num;
      var result = new AVal;
      lhs.propagate(new IsAdded(rhs, result));
      rhs.propagate(new IsAdded(lhs, result));
      return result;
    }
    var isBool = binopIsBoolean(node.operator);
    if (!isBool) { setHint(lhs, _num); setHint(rhs, _num); }
    return isBool ? _bool : _num;
  },
  AssignmentExpression: function(node, scope, c) {
    var rhs = runInfer(node.right, scope, c, lvalName(node.left));
    if (node.operator != "=" && node.operator != "+=")
      rhs = _num;

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
    return this.CallExpression(node, scope, c, true);
  },
  CallExpression:function(node, scope, c, isNew) {
    var callee, self, args = [];
    if (isNew) {
      callee = runInfer(node.callee, scope, c);
      self = getProp(callee, "prototype");
    } else if (node.callee.type == "MemberExpression") {
      self = runInfer(node.callee.object, scope, c);
      callee = getProp(self, propName(node.callee, scope, c));
    } else {
      callee = runInfer(node.callee, scope, c)
    }
    if (node.arguments) for (var i = 0; i < node.arguments.length; ++i)
      args.push(runInfer(node.arguments[i], scope, c));
    var retval = callee.retval || (callee.retval = new AVal);
    callee.propagate(new IsCallee(self, args, retval));
    return isNew ? self : retval;
  },
  MemberExpression: function(node, scope, c) {
    return getProp(runInfer(node.object, scope, c), propName(node, scope, c));
  },
  Identifier: function(node, scope) {
    var found = lookup(node.name, scope);
    if (!found) found = topScope(scope).vars[node.name] = new Var();
    return found.aval;
  },
  ThisExpression: function(node, scope, c) {
    for (var s = scope; !s.self; s = s.prev) {}
    return s ? s.self : _undef;
  },
  Literal: function(node, scope) {
    return literalType(node.value);
  }
};

function runInfer(node, scope, c) {
  return inferExprVisitor[node.type](node, scope, c);
}

var inferWrapper = walk.make({
  Expression: runInfer,
  
  ScopeBody: function(node, scope, c) { c(node, node.scope || scope); },

  FunctionDeclaration: function(node, scope, c) {
    var inner = node.body.scope;
    var aval = new AVal(new Fn(node.id.name, inner.self, inner.argvals, inner.retval));
    assignVar(node.id.name, scope, aval);
    c(node.body, scope, "ScopeBody");
  },

  VariableDeclaration: function(node, scope, c) {
    for (var i = 0; i < node.declarations.length; ++i) {
      var decl = node.declarations[i];
      if (decl.init) assignVar(decl.id.name, scope, runInfer(decl.init, scope, c));
    }
  },

  ReturnStatement: function(node, scope, c) {
    if (node.argument)
      runInfer(node.argument, scope, c).propagate(scope.retval);
  }
});

var analyze = exports.analyze = function(file) {
  var text = fs.readFileSync(file, "utf8");
  var ast = acorn.parse(text);
  walk.recursive(ast, cx.topScope, null, scopeGatherer);
  walk.recursive(ast, cx.topScope, null, inferWrapper);
  return {ast: ast, text: text, scope: cx.topScope, file: file};
}
