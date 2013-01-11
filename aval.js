// Abstract types

var hop = Object.prototype.hasOwnProperty;
function add(obj, props) {
  for (var p in props) if (hop.call(props, p)) obj[p] = props[p];
  return obj;
}

var flag_recGuard = 1;

var AVal = exports.AVal = function(type) {
  this.types = [];
  this.forward = [];
  this.flags = 0;
  if (type) type.propagate(this);
};

add(AVal.prototype, {
  addType: function(type) {
    for (var i = 0; i < this.types.length; ++i)
      if (real(this.types[i]) == type) return;
    this.types.push(type);
    for (var i = 0; i < this.forward.length; ++i)
      this.forward[i].addType(type);
  },
  propagate: function(c) {
    this.forward.push(c);
    for (var i = 0; i < this.types.length; ++i)
      c.addType(real(this.types[i]));
  },
  display: function(cx) {
    if (this.flags & flag_recGuard) return "<R>";
    this.flags |= flag_recGuard;
    var types = this.types.map(function(t) { return t.display(cx); });
    types.sort();
    var retval = types.length ? types.join(" | ") : this.guessType(cx);
    this.flags &= ~flag_recGuard;
    return retval;
  },
  guessType: function(cx) {
    if (this.hint) return this.hint.display(cx);

    var props = Object.create(null), retval = "<?>";
    for (var i = 0; retval == "<?>" && i < this.forward.length; ++i) {
      var propagate = this.forward[i];
      if (propagate instanceof AVal) {
        retval = propagate.display(cx);
      } else if (propagate instanceof PropIsSubset || propagate instanceof PropHasSubset) {
        var cur = props[propagate.prop];
        if (!cur || cur == "<?>")
          props[propagate.prop] = propagate.target.display(cx);
      } else if (propagate instanceof IsCallee) {
        retval = new Fn(cx, propagate.self, propagate.args, propagate.retval).display(cx);
      } else if (propagate instanceof IsAdded) {
        retval = propagate.other.display(cx);
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

var real = exports.real = function(type) {
  while (type.replaced) type = type.replaced;
  return type;
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
  display: function() { return this.name; },
  propagate: function(c) { c.addType(this); }
});

var _num = exports._num = new Prim("number");
var _str = exports._str = new Prim("string");
var _bool = exports._bool = new Prim("bool");
var _null = exports._null = new Prim("null");
var _undef = exports._null = new Prim("undefined");

var Obj = exports.Obj = function(cx, props) {
  this.props = Object.create(null);
  this.cx = cx;
  this.id = cx.nextObjId++;
  cx.objTypes.push(this);
  this.replaced = null;
  if (props) for (prop in props) this.addProp(prop, props[prop]);
};
add(Obj.prototype, {
  display: function(cx) {
    var props = [];
    for (var prop in this.props)
      props.push(prop + ": " + this.props[prop].display(cx));
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
    var found = this.cx.objProps[prop];
    if (found) found.push(this);
    else this.cx.objProps[prop] = [this];
  },
  propagate: function(c) { c.addType(this); },
  merge: function(other) {
    if (other == this) return;
    this.replaced = other;
    this.props = null;
    for (var p in this.props) {
      var av = this.props[p];
      if (p in other.props)
        connect(av, other.props[p]);
      else
        other.addProp(prop, av);
      rmElt(cx.objProps[p], this);
    }
    rmElt(cx.objTypes, this);
  }
});

var Fn = exports.Fn = function(cx, self, args, retval) {
  Obj.call(this, cx);
  this.self = self;
  this.args = args;
  this.retval = retval;
};
Fn.prototype = add(Object.create(Obj.prototype), {
  display: function(cx) {
    var str = "fn(" + this.args.map(function(x) { return x.display(cx); }).join(", ") + ")";
    var rettype = this.retval.display(cx);
    if (rettype != "<?>") str += " -> " + rettype;
    return str;
  },
  merge: function(other) {
    Obj.prototype.merge.call(this, other);
    connect(this.retval, other.retval);
    if (this.self && other.self)
      connect(this.self, other.self);
    else if (this.self)
      other.self = this.self;
    for (var i = 0; i < this.args.length; ++i) {
      var otherArg = other.args[i];
      if (otherArg)
        connect(this.args[i], otherArg);
      else
        other.args[i] = this.args[i];
    }
  }
});

exports.Context = function() {
  this.objTypes = [];
  this.objProps = Object.create(null);
};

exports.display = function(cx, av) {
  return av.display(cx);
};

/*var Object_prototype = new Obj(null, props({
  toString: Fn([], _str),
  valueOf: Fn([], _num),
  hasOwnProperty: Fn([_str], _bool),
  propertyIsEnumerable: Fn([_str], _bool)
}));*/
