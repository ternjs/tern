// Abstract types

var hop = Object.prototype.hasOwnProperty;
function add(obj, props) {
  for (var p in props) if (hop.call(props, p)) obj[p] = props[p];
  return obj;
}

var AVal = exports.AVal = function(type) {
  this.types = [];
  this.forward = [];
  if (type) this.addType(type);
};
add(AVal.prototype, {
  toString: function() {
    var types = this.types.map(function(t) { return t.toString(); });
    types.sort();
    return types.join(" | ") || "<empty>"; 
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
  }
});

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

var Obj = exports.Obj = function(props) {
  this.props = Object.create(null);
  if (props) add(this.props, props);
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
    return this.props[prop] = new AVal();
  },
  propagate: Prim.prototype.propagate
});

var Fn = exports.Fn = function(self, args, retval) {
  Obj.call(this);
  this.self = self;
  this.args = args;
  this.retval = retval;
  this.proto = null;
};
Fn.prototype = add(Object.create(Obj.prototype), {
  toString: function() {
    var str = "fn(" + this.args.join(", ") + ")";
    if (this.retval.types.length) str += " -> " + this.retval + ")";
    return str;
  }
});

/*var Object_prototype = new Obj(null, props({
  toString: Fn([], _str),
  valueOf: Fn([], _num),
  hasOwnProperty: Fn([_str], _bool),
  propertyIsEnumerable: Fn([_str], _bool)
}));*/
