// Abstract types

var hop = Object.prototype.hasOwnProperty;
function add(obj, props) {
  for (var p in props) if (hop.call(props, p)) obj[p] = props[p];
  return obj;
}

var AVal = exports.AVal = function(type) {
  this.types = [];
  this.forward = [];
  if (type) type.propagate(this);
};
add(AVal.prototype, {
  toString: function() {
    var types = this.types.map(function(t) { return t.toString(); });
    types.sort();
    return types.join(" | ") || this.guessType() || "<?>"; 
  },
  addType: function(type) {
    for (var i = 0; i < this.types.length; ++i)
      if (this.types[i] == type) return;
    this.types.push(type);
    for (var i = 0; i < this.forward.length; ++i)
      this.forward[i].addType(type);
  },
  propagate: function(c) {
    this.forward.push(c);
    for (var i = 0; i < this.types.length; ++i)
      c.addType(this.types[i]);
  },
  guessType: function() {
    if (this.hint) return this.hint.toString();
    var props = Object.create(null);
    for (var i = 0; i < this.forward.length; ++i) {
      var propagate = this.forward[i];
      if (propagate instanceof AVal) {
        var found = propagate.toString();
        if (found) return found;
      } else if (propagate instanceof PropIsSubset || propagate instanceof PropHasSubset) {
        var cur = props[propagate.prop];
        if (cur == "<?>") cur = null;
        props[propagate.prop] = cur || propagate.target.toString();
      } else if (propagate instanceof IsCallee) {
        return new Fn(propagate.self, propagate.args, propagate.retval).toString();
      } else if (propagate instanceof IsAdded) {
        return propagate.other.toString();
      }
    }
    // FIXME guess full obj type based on props
    var propStrs = [];
    for (var p in props) propStrs.push(p + ": " + props[p]);
    propStrs.sort();
    if (propStrs.length) return "{" + propStrs.join(", ") + "}";
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
    type.ensureProp(this.prop).propagate(this.target);
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
  if (type == aval._str)
    this.target.addType(aval._str);
  else if (type == aval._num && hasType(aval._num, other))
    this.target.addType(aval._num);
};

// FIXME handle reads from prototypes of primitive types (str.slice)
// somehow

function Prim(name) {
  this.name = name;
}
add(Prim.prototype, {
  toString: function() { return this.name; },
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
  ensureProp: function(prop) {
    var found = this.props[prop];
    if (found) return found;
    var av = new AVal;
    this.addProp(prop, av);
    return av;
  },
  addProp: function(prop, val) {
    this.props[prop] = val;
    var found = this.cx.objProps[prop];
    if (found) found.push(this);
    else this.cx.objProps[prop] = [this];
  },
  propagate: Prim.prototype.propagate
});

var Fn = exports.Fn = function(cx, self, args, retval) {
  Obj.call(this, cx);
  this.self = self;
  this.args = args;
  this.retval = retval;
  this.proto = null;
};
Fn.prototype = add(Object.create(Obj.prototype), {
  toString: function() {
    var str = "fn(" + this.args.join(", ") + ")";
    if (this.retval.types.length) str += " -> " + this.retval;
    return str;
  }
});

exports.Context = function() {
  this.objTypes = [];
  this.objProps = Object.create(null);
};

/*var Object_prototype = new Obj(null, props({
  toString: Fn([], _str),
  valueOf: Fn([], _num),
  hasOwnProperty: Fn([_str], _bool),
  propertyIsEnumerable: Fn([_str], _bool)
}));*/
