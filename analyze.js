var acorn = require("acorn"), walk = require("acorn/util/walk");
var fs = require("fs");

var aval = require("./aval");
var AVal = aval.AVal, Obj = aval.Obj, Fn = aval.Fn;

function Var(node) {
  this.node = node;
  this.aval = new AVal();
}
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

var scopeGatherer = walk.make({
  Function: function(node, scope, c) {
    var inner = node.body.scope = new Scope(scope), argvals = inner.argvals = [];
    for (var i = 0; i < node.params.length; ++i) {
      var param = node.params[i], v = new Var(param);
      inner.vars[param.name] = v;
      argvals.push(v.aval);
    }
    var retval = inner.retval = new AVal();
    if (node.id) {
      var decl = node.type == "FunctionDeclaration";
      (decl ? scope : inner).vars[node.id.name] = new Var(node.id);
    }
    c(node.body, inner, "ScopeBody");
  },
  TryStatement: function(node, scope, c) {
    c(node.block, scope, "Statement");
    for (var i = 0; i < node.handlers.length; ++i) {
      var handler = node.handlers[i], inner = handler.body.scope = new Scope(scope);
      inner.vars[handler.param.name] = new Var(handler.param);
      c(handler.body, inner, "ScopeBody");
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

function isSubset(other) { this.other = other; }
isSubset.prototype.newType = function(type) { this.other.addType(type); };

function propIsSubset(prop, other) { this.other = other; this.prop = prop; }
propIsSubset.prototype.newType = function(type) {
  if (type.ensureProp)
    type.ensureProp(this.prop).addC(new isSubset(other));
};

function propHasSubset(prop, other) { this.other = other; this.prop = prop; }
propHasSubset.prototype.newType = function(type) {
  if (type.ensureProp)
    this.other.addC(new isSubset(type.ensureProp(this.prop)));
};

// FIXME retval avals could be reused
function isCallee(args, retval) { this.args = args; this.retval = retval; }
isCallee.prototype.newType = function(type) {
  if (!type.args) return;
  for (var i = 0, e = Math.min(this.args.length, type.args.length); i < e; ++i)
    this.args[i].addC(new isSubset(type.args[i]));
  type.retval.addC(new isSubset(this.retval));
};

function assignVar(name, scope, aval) {
  var found = lookup(name, scope);
  if (!found) found = topScope(scope).vars[name] = new Var();
  aval.addC(new isSubset(found.aval));
  return found.aval;
}

function propName(node, scope, c) {
  if (node.type == "Identifier") return node.name;
  if (node.type == "Literal" && typeof node.value == "string") return node.value;
  runInfer(node, scope, c);
  return "<i>";
}

function unopResultType(op) {
  switch (op) {
  case "+": case "-": case "~": return aval._num;
  case "!": return aval._bool;
  case "typeof": return aval._str;
  case "void": case "delete": return aval._undef;
  }
}
function binopResultType(op) {
  switch (op) {
  case "==": case "!=": case "===": case "!==": case "<": case ">": case ">=": case "<=":
  case "in": case "instanceof": return aval._bool;
  default: return aval._num;
  }
}
function literalType(val) {
  switch (typeof val) {
  case "boolean": return aval._bool;
  case "number": return aval._num;
  case "string": return aval._str;
  case "object":
    if (!val) return aval._null;
    return new Obj(); // FIXME point to single regexp obj
  }
}


var inferExprVisitor = {
  ArrayExpression: function(node, scope, c) {
    var eltval = new AVal();
    for (var i = 0; i < node.elements.length; ++i) {
      var elt = node.elements[i];
      if (elt) eltval.addC(new isSubset(runInfer(elt, scope, c)));
    }
    return new Obj({"<i>": eltval});
  },
  ObjectExpression: function(node, scope, c) {
    var props = {};
    for (var i = 0; i < node.properties.length; ++i) {
      var p = node.properties[i];
      props[p.key.name] = runInfer(p.value, scope, c);
    }
    return new Obj(props);
  },
  FunctionExpression: function(node, scope, c) {
    var inner = node.body.scope;
    var aval = new AVal(new Fn(inner.argvals, inner.retval));
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
    return aval._num;
  },
  BinaryExpression: function(node, scope, c) {
    runInfer(node.left, scope, c);
    runInfer(node.right, scope, c);
    // FIXME handle + more precisely
    return binopResultType(node.operator);
  },
  AssignmentExpression: function(node, scope, c) {
    var rhs = runInfer(node.right, scope, c), out;
    if (node.left.type == "Identifier") {
      out = assignVar(node.left.name, scope, rhs);
    } else { // MemberExpression
      out = new AVal();
      out.addC(new isSubset(rhs));
      var obj = runInfer(node.left.object, scope, c);
      if (!obj) console.log(node.left.object);
      obj.addC(new propHasSubset(propName(node.left.property, scope, c), out));
    }
    // FIXME handle +
    if (node.operator != "=") out.addType(aval._num);
    return out;
  },
  LogicalExpression: function(node, scope, c) {
    var aval = new AVal();
    aval.addC(new isSubset(runInfer(node.left, scope, c)));
    aval.addC(new isSubset(runInfer(node.right, scope, c)));
    return aval;
  },
  ConditionalExpression: function(node, scope, c) {
    runInfer(node.test, scope, c);
    var aval = new AVal();
    aval.addC(new isSubset(runInfer(node.consequent, scope, c)));
    aval.addC(new isSubset(runInfer(node.alternate, scope, c)));
    return aval;
  },
  NewExpression: function(node, scope, c) {
    // FIXME
    return this.CallExpression(node, scope, c);
  },
  CallExpression:function(node, scope, c) {
    var callee = runInfer(node.callee, scope, c), args = [];
    if (node.arguments) for (var i = 0; i < node.arguments.length; ++i)
      args.push(runInfer(node.arguments[i], scope, c));
    var retval = new AVal();
    callee.addC(new isCallee(args, retval));
    return retval;
  },
  MemberExpression: function(node, scope, c) {
    var obj = runInfer(node.object, scope, c);
    var result = new AVal();
    result.addC(new propIsSubset(propName(node.property, scope, c), result));
    return result;
  },
  Identifier: function(node, scope) {
    var found = lookup(node.name, scope);
    if (!found) found = topScope(scope).vars[node.name] = new Var();
    return found.aval;
  },
  ThisExpression: function(node, scope, c) {
    // FIXME
    return new AVal();
  },
  Literal: function(node) {
    return new literalType(node.value);
  }
};

function runInfer(node, scope, c) {
  return inferExprVisitor[node.type](node, scope, c);
}

var inferWrapper = walk.make({
  Expression: runInfer,
  
  ScopeBody: function(node, scope, c) { c(node, node.scope); },

  FunctionDeclaration: function(node, scope, c) {
    var inner = node.body.scope;
    var aval = new AVal(new Fn(inner.argvals, inner.retval));
    assignVar(node.id.name, scope, aval);
    c(node.body, scope, "ScopeBody");
  },

  VariableDeclaration: function(node, scope, c) {
    for (var i = 0; i < node.declarations.length; ++i) {
      var decl = node.declarations[i];
      if (decl.init) assignVar(decl.id.name, scope, runInfer(decl.init, scope, c));
    }
  },

  // FIXME handle implicit return somehow? Or simply default to
  // undefined when nothing else is filled in.
  ReturnStatement: function(node, scope, c) {
    for (var s = scope; !s.retval; s = s.prev) {}
    s.retval.addType(node.argument ? runInfer(node.argument, scope, c) : aval._undef);
  }
});

function infer(ast, env) {
  walk.recursive(ast, env, null, inferWrapper);
}

function analyze(ast) {
  var top = new Scope();
  walk.recursive(ast, top, null, scopeGatherer);
  infer(ast, top);
  return top;
}

var env = analyze(acorn.parse(require("fs").readFileSync(process.argv[2], "utf8")));
for (var global in env.vars)
  console.log(global + " = " + env.vars[global].aval.toString());
