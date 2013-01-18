var acorn = require("acorn"), walk = require("acorn/util/walk");
var fs = require("fs");

var flag_recGuard = 1, flag_initializer = 2;

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
      type.findProp(this.prop).propagate(this.target);
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
    if (type == cx.prim.str)
      this.target.addType(cx.prim.str);
    else if (type == cx.prim.num && hasType(cx.prim.num, this.other))
      this.target.addType(cx.prim.num);
  },
  typeHint: function(maxDepth) {
    return this.other.toString(maxDepth);
  }
};

// TYPE OBJECTS

// FIXME handle reads from prototypes of primitive types (str.slice)
// somehow

function Prim(proto, name) { this.name = name; }
Prim.prototype = {
  toString: function() { return this.name; },
  propagate: function(c) { c.addType(this); },
  sameType: function(other) { return other == this; }
};

function hop(obj, prop) {
  return Object.prototype.hasOwnProperty.call(obj, prop);
}

function Obj(proto, name) {
  this.props = Object.create(proto && proto.props);
  this.name = name;
}
Obj.prototype = {
  toString: function(maxDepth) {
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
  },
  ensureProp: function(prop, alsoProto) {
    var found = this.props[prop];
    if (found && (alsoProto || hop(this.props, prop))) return found;
    var av = new AVal;
    this.addProp(prop, av);
    return av;
  },
  findProp: function(prop) {
    return this.ensureProp(prop, true);
  },
  addProp: function(prop, val) {
    this.props[prop] = val;
    // FIXME should objProps also list proto's props? maybe move back to objList?
    if (this.prev) return; // If this is a scope, it shouldn't be registered
    var found = cx.objProps[prop];
    if (found) found.push(this);
    else cx.objProps[prop] = [this];
  },
  propagate: function(c) { c.addType(this); }
};

Obj.fromInitializer = function(props, name) {
  var types = props.length && cx.objProps[props[0].name];
  // FIXME check prototype
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
  var obj = new Obj(cx.protos.Object, name);
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

// FIXME save argument names

function Fn(name, self, args, retval) {
  Obj.call(this, cx.protos.Function, name);
  this.self = self;
  this.args = args;
  this.retval = retval;
}
Fn.prototype = Object.create(Obj.prototype);
Fn.prototype.toString = function(maxDepth) {
  if (maxDepth) maxDepth--;
  var str = "fn(" + this.args.map(function(x) {
    var name = x.name, typ = x.toString(maxDepth);
    if (name && typeof name != "string") name = name.name;
    if (name) return name + ": " + typ;
    else return typ;
  }).join(", ") + ")";
  var rettype = this.retval.toString(maxDepth);
  if (rettype != "<?>") str += " -> " + rettype;
  return str;
};
Fn.prototype.ensureProp = function(prop, alsoProto) {
  var newProto = this.name && prop == "prototype" && !("prototype" in this.props);
  var retval = Obj.prototype.ensureProp.call(this, prop, alsoProto);
  if (newProto) retval.propagate(new IsProto(this.name));
  return retval;
};

function Arr(contentType) {
  Obj.call(this, cx.protos.Array);
  if (contentType) contentType.propagate(this.ensureProp("<i>"));
}
Arr.prototype = Object.create(Obj.prototype);
Arr.prototype.toString = function(maxDepth) {
  if (maxDepth) maxDepth--;
  return "[" + this.findProp("<i>").toString(maxDepth) + "]";
};

// INFERENCE CONTEXT

function Context(type) {
  this.type = type;
  this.objProps = Object.create(null);
  this.topScope = new Scope();
  this.protos = Object.create(null);
  this.prim = Object.create(null);
  exports.withContext(this, function() {
    initJSContext();
    if (type == "browser") initBrowserContext();
    else if (type == "node") initNodeContext();
  });
}

var cx = null;

exports.withContext = function(context, f) {
  var old = cx;
  cx = context || new Context("browser");
  try { return f(); }
  finally { cx = old; }
};

// SCOPES

// FIXME local arguments variable
function Scope(prev) {
  Obj.call(this, null, prev ? null : "global");
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
    var inner = node.body.scope = new Scope(scope), argvals = inner.argvals = [];
    for (var i = 0; i < node.params.length; ++i) {
      var param = node.params[i], b = inner.ensureProp(param.name);
      b.name = param;
      argvals.push(b);
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

function assignVar(name, scope, aval) {
  aval.propagate(scope.lookup(name, true));
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
    // FIXME return something for regexps
    return new Obj(cx.protos.RegExp);
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
    return cx.prim.num;
  },
  BinaryExpression: function(node, scope, c) {
    var lhs = runInfer(node.left, scope, c);
    var rhs = runInfer(node.right, scope, c);
    if (node.operator == "+") {
      if (hasType(cx.prim.str, lhs) || hasType(cx.prim.str, rhs)) return cx.prim.str;
      if (hasType(cx.prim.num, lhs) && hasType(cx.prim.num, rhs)) return cx.prim.num;
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
    return scope.lookup(node.name, true);
  },
  ThisExpression: function(node, scope, c) {
    for (var s = scope; !s.self; s = s.prev) {}
    return s ? s.self : cx.prim.undef;
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
};

// CONTEXT POPULATING

function parseType(spec, name, self) {
  var pos = 0;
  function parseArgs() {
    var args = [];
    while (spec.charAt(pos) != ")") {
      var colon = spec.indexOf(": ", pos), argname, aval;
      if (colon != -1) {
        argname = spec.slice(pos, colon);
        if (/^[\w?]+$/.test(argname))
          pos = colon + 2;
        else
          argname = null;
      }
      args.push(aval = new AVal(inner()));
      if (argname) aval.name = argname;
      if (spec.indexOf(", ", pos) == pos) pos += 2;
    }
    ++pos;
    return args;
  }
  function inner(name, self) {
    if (spec.indexOf("fn(", pos) == pos) {
      pos += 3;
      var args = parseArgs(), retType;
      if (spec.indexOf(" -> ", pos) == pos) {
        pos += 4;
        retType = inner();
      }
      return new Fn(name, new AVal(self), args, new AVal(retType));
    } else if (spec.indexOf("ctor(", pos) == pos) {
      pos += 5;
      return new Fn(name, new AVal(cx.protos[name]), parseArgs(), new AVal);
    } else if (spec.charAt(pos) == "<") {
      var end = spec.indexOf(">", pos), word = spec.slice(pos + 1, end);
      pos = end + 1;
      switch (word) {
      case "number": return cx.prim.num;
      case "string": return cx.prim.str;
      case "bool": return cx.prim.bool;
      case "null": return cx.prim.null;
      case "undefined": return cx.prim.undef;
      case "?": return null;
      }
    } else if (spec.charAt(pos) == "[") {
      ++pos;
      var arr = new Arr(inner());
      if (spec.charAt(pos) == "]") {
        ++pos;
        return arr;
      }
    } else {
      var word = "", ch;
      while ((ch = spec.charAt(pos)) && /\w/.test(ch)) { word += ch; ++pos; }
      if (word in cx.protos) return cx.protos[word];
    }
    throw new Error("Unrecognized type spec: " + spec);
  }
  return inner(name, self);
}

function populate(obj, props) {
  for (var prop in props) if (hop(props, prop)) {
    var v = obj.ensureProp(prop), spec = props[prop];
    var t = typeof spec == "string" ? parseType(spec, prop, obj) : spec;
    if (t) t.propagate(v);
  }
  return obj;
}

function initJSContext() {
  var objProto = cx.protos.Object = new Obj(null, "Object");
  cx.protos.Array = new Obj(objProto, "Array");
  cx.protos.Function = new Obj(objProto, "Function");
  cx.protos.String = new Obj(objProto, "String");
  cx.protos.Number = new Obj(objProto, "Number");
  cx.protos.Date = new Obj(objProto, "Date")
  cx.prim.str = new Prim(cx.protos.String, "<string>");
  cx.prim.bool = new Prim(cx.protos.Object, "<bool>");
  cx.prim.num = new Prim(cx.protos.Number, "<number>");
  cx.prim.null = new Prim(null, "<null>");
  cx.prim.undef = new Prim(null, "<undefined>");

  populate(objProto, {
    toString: "fn() -> <string>",
    toLocaleString: "fn() -> <string>",
    valueOf: "fn() -> <number>",
    hasOwnProperty: "fn(prop: <string>) -> <bool>",
    propertyIsEnumerable: "fn(prop: <string>) -> <bool>"
  });
  // FIXME use information about array elt type to fill in types somehow
  // FIXME type parameters would be neat -- i.e the inner type of map's result is the retval of the argument function
  populate(cx.protos.Array, {
    length: "<number>",
    concat: "fn(other: [<?>]) -> [<?>]",
    join: "fn(separator?: <string>) -> <string>",
    splice: "fn(pos: <number>, amount: <number>)",
    pop: "fn() -> <?>",
    push: "fn(newelt: <?>) -> <number>",
    shift: "fn() -> <?>",
    unshift: "fn(newelt: <?>) -> <number>",
    slice: "fn(from: <number>, to?: <number>) -> [<?>]",
    reverse: "fn()",
    sort: "fn(compare?: fn(a: <?>, b: <?>) -> <number>)",
    indexOf: "fn(elt: <?>, from?: <number>) -> <number>",
    lastIndexOf: "fn(elt: <?>, from?: <number>) -> <number>",
    every: "fn(test: fn(elt: <?>, i: <number>) -> <bool>) -> <bool>",
    some: "fn(test: fn(elt: <?>, i: <number>) -> <bool>) -> <bool>",
    filter: "fn(test: fn(elt: <?>, i: <number>) -> <bool>) -> [<?>]",
    forEach: "fn(f: fn(elt: <?>, i: <number>))",
    map: "fn(f: fn(elt: <?>, i: <number>) -> <?>) -> [<?>]",
    reduce: "fn(combine: fn(sum: <?>, elt: <?>, i: <number>) -> <?>, init?: <?>) -> <?>",
    reduceRight: "fn(combine: fn(sum: <?>, elt: <?>, i: <number>) -> <?>, init?: <?>) -> <?>"
  });
  populate(cx.protos.Function, {
    apply: "fn(this: <?>, args: [<?>])",
    call: "fn(this: <?>)",
    bind: "fn(this: <?>)"
  });
  cx.protos.RegExp = populate(new Obj(objProto, "RegExp"), {
    exec: "fn(input: <string>) -> [<string>]",
    compile: "fn(source: <string>, flags?: <string>)",
    test: "fn(input: <string>) -> <bool>",
    global: "<bool>",
    ignoreCase: "<bool>",
    multiline: "<bool>",
    source: "<string>",
    lastIndex: "<number>"
  });
  populate(cx.protos.String, {
    length: "<number>",
    charAt: "fn(i: <number>) -> <string>",
    charCodeAt: "fn(i: <number>) -> <number>",
    indexOf: "fn(char: <string>, from?: <number>) -> <number>",
    lastIndexOf: "fn(char: <string>, from?: <number>) -> <number>",
    substring: "fn(from: <number>, to?: <number>) -> <string>",
    substr: "fn(from: <number>, length?: <number>) -> <string>",
    slice: "fn(from: <number>, to?: <number>) -> <string>",
    trim: "fn() -> <string>",
    trimLeft: "fn() -> <string>",
    trimRight: "fn() -> <string>",
    toUpperCase: "fn() -> <string>",
    toLowerCase: "fn() -> <string>",
    toLocaleUpperCase: "fn() -> <string>",
    toLocaleLowerCase: "fn() -> <string>",
    split: "fn(pattern: <string>) -> [<string>]",
    concat: "fn(other: <string>) -> <string>",
    localeCompare: "fn(other: <string>) -> <number>",
    match: "fn(pattern: RegExp) -> [<string>]",
    replace: "fn(pattern: RegExp, replacement: <string>) -> <string>",
    search: "fn(pattern: RegExp) -> <number>"
  });
  populate(cx.protos.Number, {
    toString: "fn(radix?: <number>) -> <string>",
    toFixed: "fn(digits: <number>) -> <string>",
    toExponential: "fn(digits: <number>) -> <string>"
  });
  populate(cx.protos.Date, {
    toUTCString: "fn() -> <string>",
    toISOString: "fn() -> <string>",
    toDateString: "fn() -> <string>",
    toTimeString: "fn() -> <string>",
    toLocaleString: "fn() -> <string>",
    toLocaleDateString: "fn() -> <string>",
    toLocaleTimeString: "fn() -> <string>",
    getTime: "fn() -> <number>",
    getFullYear: "fn() -> <number>",
    getYear: "fn() -> <number>",
    getMonth: "fn() -> <number>",
    getUTCMonth: "fn() -> <number>",
    getDate: "fn() -> <number>",
    getUTCDate: "fn() -> <number>",
    getDay: "fn() -> <number>",
    getUTCDay: "fn() -> <number>",
    getHours: "fn() -> <number>",
    getUTCHours: "fn() -> <number>",
    getMinutes: "fn() -> <number>",
    getUTCMinutes: "fn() -> <number>",
    getSeconds: "fn() -> <number>",
    getUTCSeconds: "fn() -> <number>",
    getMilliseconds: "fn() -> <number>",
    getUTCMilliseconds: "fn() -> <number>",
    getTimezoneOffset: "fn() -> <number>",
    setTime: "fn(date: Date) -> <number>",
    setFullYear: "fn(year: <number>) -> <number>",
    setUTCFullYear: "fn(year: <number>) -> <number>",
    setMonth: "fn(month: <number>) -> <number>",
    setUTCMonth: "fn(month: <number>) -> <number>",
    setDate: "fn(day: <number>) -> <number>",
    setUTCDate: "fn(day: <number>) -> <number>",
    setHours: "fn(hour: <number>) -> <number>",
    setUTCHours: "fn(hour: <number>) -> <number>",
    setMinutes: "fn(min: <number>) -> <number>",
    setUTCMinutes: "fn(min: <number>) -> <number>",
    setSeconds: "fn(sec: <number>) -> <number>",
    setUTCSeconds: "fn(sec: <number>) -> <number>",
    setMilliseconds: "fn(ms: <number>) -> <number>",
    setUTCMilliseconds: "fn(ms: <number>) -> <number>"
  });
  cx.protos.Error = populate(new Obj(objProto, "Error"), {
    name: "<string>",
    message: "<string>"
  });

  cx.topScope.props = Object.create(objProto.props);
  var errC = parseType("ctor(message: <string>)", "Error");
  populate(cx.topScope, {
    Infinity: "<number>",
    undefined: "<undefined>",
    NaN: "<number>",
    Object: populate(parseType("ctor()", "Object"), {
      getPrototypeOf: "fn(obj: <?>) -> <?>",
      create: "fn(proto: <?>) -> <?>",
      defineProperties: "fn(obn: <?>, props: <?>)",
      keys: "fn(obj: <?>) -> [<string>]"
    }),
    Function: "ctor(body: <string>)",
    Array: populate(parseType("ctor(size: <number>)", "Array"), {
      isArray: "fn(value: <?>) -> <bool>"
    }),
    String: populate(parseType("ctor(value: <?>) -> <string>", "String"), {
      fromCharCode: "fn(code: <number>) -> <string>"
    }),
    Number: populate(parseType("ctor(value: <?>) -> <number>", "Number"), {
      MAX_VALUE: "<number>",
      MIN_VALUE: "<number>",
      POSITIVE_INFINITY: "<number>",
      NEGATIVE_INFINITY: "<number>"
    }),
    RegExp: "ctor(source: <string>, flags?: <string>)",
    Date: populate(parseType("ctor(ms: <number>)", "Date"), {
      parse: "fn(source: <string>) -> Date",
      UTC: "fn(year: <number>, month: <number>, date: <number>, hour?: <number>, min?: <number>, sec?: <number>, ms?: <number>) -> <number>",
      now: "fn() -> <number>"
    }),
    Boolean: "fn(value: <?>) -> <bool>",
    Error: errC, SyntaxError: errC, ReferenceError: errC, URIError: errC, EvalError: errC, RangeError: errC,
    parseInt: "fn(string: <string>, radix?: <number>) -> <number>",
    parseFloat: "fn(string: <string>) -> <number>",
    isNaN: "fn(value: <number>) -> <bool>",
    eval: "fn(code: <string>) -> <?>",
    encodeURI: "fn(uri: <string>) -> <string>",
    encodeURIComponent: "fn(uir: <string>) -> <string>",
    decodeURI: "fn(uri: <string>) -> <string>",
    decodeURIComponent: "fn(uri: <string>) -> <string>",
    Math: populate(new Obj(objProto, "Math"), {
      E: "<number>", LN2: "<number>", LN10: "<number>", LOG2E: "<number>", LOG10E: "<number>",
      SQRT1_2: "<number>", SQRT2: "<number>", PI: "<number>",
      abs: "fn(<number>) -> <number>",
      cos: "fn(<number>) -> <number>",
      sin: "fn(<number>) -> <number>",
      tan: "fn(<number>) -> <number>",
      acos: "fn(<number>) -> <number>",
      asin: "fn(<number>) -> <number>",
      atan: "fn(<number>) -> <number>",
      atan2: "fn(<number>, <number>) -> <number>",
      ceil: "fn(<number>) -> <number>",
      floor: "fn(<number>) -> <number>",
      round: "fn(<number>) -> <number>",
      exp: "fn(<number>) -> <number>",
      log: "fn(<number>) -> <number>",
      sqrt: "fn(<number>) -> <number>",
      pow: "fn(<number>, <number>) -> <number>",
      max: "fn(<number>, <number>) -> <number>",
      min: "fn(<number>, <number>) -> <number>",
      random: "fn() -> <number>"
    }),
    JSON: populate(new Obj(objProto, "JSON"), {
      parse: "fn(json: <string>) -> <?>",
      stringify: "fn(value: <?>) -> <string>"
    })
  });
}

function initBrowserContext() {
  // FIXME
}

function initNodeContext() {
  // FIXME
}
