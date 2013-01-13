// Abstract types

var hop = Object.prototype.hasOwnProperty;
function add(obj, props) {
  for (var p in props) if (hop.call(props, p)) obj[p] = props[p];
  return obj;
}

var flag_recGuard = 1;

var AVal = exports.AVal = function(type) {
  this.types = [];
  this.scores = [];
  this.forward = [];
  this.flags = 0;
  if (type) type.propagate(this);
};

add(AVal.prototype, {
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
  toString: function() {
    if (this.flags & flag_recGuard) return "<R>";
    this.flags |= flag_recGuard;
    if (this.types.length > 3) return "<giga>";
    var types = this.types.map(function(t) { return t.toString(); });
    types.sort();
    var retval = types.length ? types.join(" | ") : this.guessType();
    this.flags &= ~flag_recGuard;
    return retval;
  },
  guessType: function() {
    if (this.hint) return this.hint.toString();

    var props = Object.create(null), retval = "<?>";
    for (var i = 0; retval == "<?>" && i < this.forward.length; ++i) {
      var propagate = this.forward[i];
      if (propagate instanceof AVal) {
        retval = propagate.toString();
      } else if (propagate instanceof PropIsSubset || propagate instanceof PropHasSubset) {
        var cur = props[propagate.prop];
        if (!cur || cur == "<?>")
          props[propagate.prop] = propagate.target.toString();
      } else if (propagate instanceof IsCallee) {
        retval = new Fn(cx, propagate.self, propagate.args, propagate.retval).toString();
      } else if (propagate instanceof IsAdded) {
        retval = propagate.other.toString();
      }
    }
    if (retval == "<?>") {
      // FIXME guess full obj type based on props
      var propStrs = [];
      for (var p in props) propStrs.push(p + ": " + props[p]);
      propStrs.sort();
      if (propStrs.length) retval = "{" + propStrs.join(", ") + "}";
    }
    return retval;
  }
});

var hasType = exports.hasType = function(type, set) {
  return set == type || set.types && set.types.indexOf(type) > -1;
}

var PropIsSubset = exports.PropIsSubset = function(prop, target) {
  this.target = target; this.prop = prop;
}
PropIsSubset.prototype.addType = function(type) {
  if (type.ensureProp)
    type.ensureProp(this.prop, true).propagate(this.target);
};

var PropHasSubset = exports.PropHasSubset = function(prop, target) {
  this.target = target; this.prop = prop;
}
PropHasSubset.prototype.addType = function(type) {
  if (type.ensureProp)
    this.target.propagate(type.ensureProp(this.prop));
};

var IsCallee = exports.IsCallee = function(self, args, retval) {
  this.self = self; this.args = args; this.retval = retval;
}
IsCallee.prototype.addType = function(fn) {
  if (!(fn instanceof Fn)) return;
  for (var i = 0, e = Math.min(this.args.length, fn.args.length); i < e; ++i)
    this.args[i].propagate(fn.args[i]);
  if (this.self) this.self.propagate(fn.self);
  fn.retval.propagate(this.retval);
};

var IsAdded = exports.IsAdded = function(other, target) {
  this.other = other; this.target = target;
}
IsAdded.prototype.addType = function(type) {
  if (type == _str)
    this.target.addType(_str);
  else if (type == _num && hasType(_num, this.other))
    this.target.addType(_num);
};

function connect(a, b) {
  a.propagate(b);
  b.propagate(a);
}

function rmElt(arr, elt) {
  for (var i = 0; i < arr.length; ++i) if (arr[i] == elt) {
    arr.splice(i, 1);
    break;
  }
}

// FIXME handle reads from prototypes of primitive types (str.slice)
// somehow

function Prim(name) {
  this.name = name;
}
add(Prim.prototype, {
  toString: function() { return this.name; },
  propagate: function(c) { c.addType(this); },
  sameType: function(other) { return other == this; }
});

var _num = exports._num = new Prim("number");
var _str = exports._str = new Prim("string");
var _bool = exports._bool = new Prim("bool");
var _null = exports._null = new Prim("null");
var _undef = exports._null = new Prim("undefined");

var Obj = exports.Obj = function(props) {
  this.props = Object.create(null);
  this.id = cx.nextObjId++;
  cx.objTypes.push(this);
  this.replaced = null;
  if (props) for (prop in props) this.addProp(prop, props[prop]);
};
add(Obj.prototype, {
  toString: function() {
    var props = [];
    for (var prop in this.props)
      props.push(prop + ": " + this.props[prop].toString());
    props.sort();
    return "{" + props.join(", ") + "}";
  },
  ensureProp: function(prop, speculative) {
    var found = this.props[prop];
    if (found) {
      if (!speculative && found.speculative) found.speculative = null;
      return found;
    }
    var av = new AVal;
    if (speculative) av.speculative = true;
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
});

function compatible(one, two) {
  if (!one.types.length || !two.types.length) return true;
  return one.isDominant(two.dominantType());
}

var Fn = exports.Fn = function(self, args, retval) {
  Obj.call(this);
  this.self = self;
  this.args = args;
  this.retval = retval;
};
Fn.prototype = add(Object.create(Obj.prototype), {
  constructor: Fn,
  toString: function() {
    var str = "fn(" + this.args.map(function(x) { return x.toString(); }).join(", ") + ")";
    var rettype = this.retval.toString();
    if (rettype != "<?>") str += " -> " + rettype;
    return str;
  }
});

var Context = exports.Context = function() {
  this.objTypes = [];
  this.objProps = Object.create(null);
};

var cx = null;

exports.withContext = function(context, f) {
  var old = cx;
  cx = context || new Context();
  try { return f(); }
  finally { cx = old; }
};

exports.cx = function() { return cx; };

/*var Object_prototype = new Obj(null, props({
  toString: Fn([], _str),
  valueOf: Fn([], _num),
  hasOwnProperty: Fn([_str], _bool),
  propertyIsEnumerable: Fn([_str], _bool)
}));*/
