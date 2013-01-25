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
    // FIXME merge strategy -- [<?>] + [<int>] = [<int>], similar for object & fn types
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

function HasName(name) { this.name = name; }
HasName.prototype = {
  addType: function(o) {
    if (o instanceof Obj && !o.name) o.name = this.name;
  },
  typeHint: function() { return this.name; }
};

function IsProto(other) { this.other = other; }
IsProto.getInstance = function(o) {
  if (!(o instanceof Obj)) return null;
  if (!o.instance)
    o.instance = new Obj(o, this.name && /\.prototype$/.test(this.name) ? this.name.slice(0, this.name.length - 10) : null);
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

// FIXME handle reads from prototypes of primitive types (str.slice)
// somehow

function Type(name) { this.name = name; }
Type.prototype = {
  propagate: function(c) { c.addType(this); },
  hasType: function(other) { return other == this; }
};

function Prim(proto, name) { this.name = name; }
Prim.prototype = Object.create(Type.prototype);
Prim.prototype.toString = function() { return this.name; };

function hop(obj, prop) {
  return Object.prototype.hasOwnProperty.call(obj, prop);
}

function Obj(proto, name) {
  if (proto === true) proto = cx.protos.Object;
  this.props = Object.create(proto && proto.props);
  this.name = name;
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
  this.addProp(prop, av);
  return av;
};
Obj.prototype.getProp = function(prop) {
  return this.ensureProp(prop, true);
};
Obj.prototype.addProp = function(prop, val) {
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
  if (newProto) retval.propagate(new HasName(this.name + ".prototype"));
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

function Context(type) {
  this.type = type;
  this.objProps = Object.create(null);
  this.topScope = new Scope();
  this.protos = Object.create(null);
  this.prim = Object.create(null);
  this.localProtos = null;
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

function lvalName(node) {
  if (node.type == "Identifier") return node.name;
  if (node.type == "MemberExpression" && node.computed) return node.property.name;
}

function setHint(aval, hint) {
  if (aval.types && !aval.types.length) aval.hint = hint;
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
  // FIXME separate between prototype and instances, but reuse
  // instance type
  NewExpression: function(node, scope, c) {
    return this.CallExpression(node, scope, c, true);
  },
  CallExpression: function(node, scope, c, isNew) {
    var callee, self, args = [];
    if (isNew) {
      callee = runInfer(node.callee, scope, c);
      self = new AVal;
      callee.getProp("prototype").propagate(new IsProto(self));
    } else if (node.callee.type == "MemberExpression") {
      self = runInfer(node.callee.object, scope, c);
      callee = self.getProp(propName(node.callee, scope, c));
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
    return runInfer(node.object, scope, c).getProp(propName(node, scope, c));
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
  parseArgs: function() {
    for (var args = [], i = 0; !this.eat(")"); ++i) {
      var colon = this.spec.indexOf(": ", this.pos), argname, aval;
      if (colon != -1) {
        argname = this.spec.slice(this.pos, colon);
        if (/^[$\w?]+$/.test(argname))
          this.pos = colon + 2;
        else
          argname = null;
      }
      args.push(aval = new AVal(this.parseType()));
      if (argname) aval.name = argname;
      this.eat(", ");
    }
    return args;
  },
  parseType: function(name, self, top) {
    if (this.eat("fn(")) {
      var args = this.parseArgs(), retType, computeRet;
      if (this.eat(" -> ")) {
        if (top && this.spec.indexOf("$", this.pos) > -1) {
          retType = null;
          computeRet = this.parseRetType();
        } else retType = this.parseType();
      }
      var fn = new Fn(name, new AVal, args, new AVal(retType));
      if (computeRet) fn.computeRet = computeRet;
      return fn;
    } else if (name && this.eat("ctor(")) {
      return new Fn(name, new AVal(cx.protos[name]), this.parseArgs(), new AVal);
    } else if (this.eat("<")) {
      var end = this.spec.indexOf(">", this.pos), word = this.spec.slice(this.pos, end);
      this.pos = end + 1;
      switch (word) {
      case "number": return cx.prim.num;
      case "string": return cx.prim.str;
      case "bool": return cx.prim.bool;
      case "null": return cx.prim.null;
      case "undefined": return cx.prim.undef;
      case "?": return null;
      }
    } else if (this.eat("[")) {
      var arr = new Arr(this.parseType());
      if (this.eat("]")) return arr;
    } else {
      var word = this.word();
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

function parseType(spec, name, self) {
  return new TypeParser(spec).parseType(name, self, true);
}

function populate(obj, props, name) {
  for (var prop in props) if (hop(props, prop) && !/^__/.test(prop)) {
    var nm = name ? name + "." + prop : prop;
    var v = obj.ensureProp(prop);
    var ty = interpret(props[prop], nm, obj);
    if (ty) ty.propagate(v);
  }
  return obj;
}

function interpret(spec, name, self) {
  if (typeof spec == "string") return parseType(spec, name, self);
  // Else, it is an object spec
  var obj;
  if (spec.__type) obj = interpret(spec.__type, name, self);
  else if (spec.__stdProto) obj = cx.protos[spec.__stdProto];
  else if (spec.__isProto) obj = cx.localProtos[spec.__isProto];
  else obj = new Obj(spec.__proto ? interpret(spec.__proto) : true, name);
  return populate(obj, spec, name);
}

function loadEnvironment(file) {
  cx.localProtos = Object.create(null);
  var info = JSON.parse(fs.readFileSync("ecma5.json"));
  var ps = info.__protos;
  if (ps) for (var name in ps) if (hop(ps, name))
    cx.localProtos[name] = interpret(ps[name], name + ".prototype");
  populate(cx.topScope, info);
}

function initJSContext() {
  cx.protos.Object = new Obj(null, "Object");
  cx.topScope.props = Object.create(cx.protos.Object.props);

  cx.protos.Array = new Obj(true, "Array.prototype");
  cx.protos.Function = new Obj(true, "Function.prototype");
  cx.protos.RegExp = new Obj(true, "RegExp.prototype");
  cx.protos.String = new Obj(true, "String.prototype");
  cx.protos.Number = new Obj(true, "Number.prototype");
  cx.protos.Boolean = new Obj(true, "Boolean.prototype");
  cx.prim.str = new Prim(cx.protos.String, "<string>");
  cx.prim.bool = new Prim(cx.protos.Boolean, "<bool>");
  cx.prim.num = new Prim(cx.protos.Number, "<number>");
  cx.prim.null = new Prim(null, "<null>");
  cx.prim.undef = new Prim(null, "<undefined>");
  
  // FIXME cache this (maybe even pre-parse/compile)
  loadEnvironment("ecma5.json");
}

function initBrowserContext() {
  // FIXME
}

function initNodeContext() {
  // FIXME
}
