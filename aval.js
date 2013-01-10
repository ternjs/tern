// Abstract types

var hop = Object.prototype.hasOwnProperty;
function add(obj, props) {
  for (var p in props) if (hop.call(props, p)) obj[p] = props[p];
}

var AVal = exports.AVal = function(type) {
  this.types = [];
  this.constraints = [];
  if (type) this.addType(type);
}
add(AVal.prototype, {
  toString: function() { return this.types.join(" | ") || "<empty>"; },
  addType: function(type) {
    for (var i = 0; i < this.types.length; ++i)
      if (this.types[i] == type) return;
    this.types.push(type);
    for (var i = 0; i < this.constraints.length; ++i)
      this.constraints[i].newType(type);
  },
  addC: function(c) {
    this.constraints.push(c);
    for (var i = 0; i < this.types.length; ++i)
      c.newType(this.types[i]);
  }
});

// FIXME handle reads from prototypes of primitive types (str.slice)
// somehow

function Prim(name) {
  this.name = name;
}
add(Prim.prototype, {
  toString: function() { return this.name; },
  addC: function(c) { c.newType(this); }
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
    var out = "{";
    for (var prop in this.props) {
      if (out.length > 1) out += ", ";
      out += prop + ": " + this.props[prop].toString();
    }
    out += "}";
    return out;
  },
  ensureProp: function(prop) {
    var found = this.props[prop];
    if (found) return found;
    return this.props[prop] = new AVal();
  },
  addC: Prim.prototype.addC
});

var Fn = exports.Fn = function(args, retval) {
  Obj.call(this);
  this.args = args;
  this.retval = retval;
};
Fn.prototype = Object.create(Obj.prototype);
Fn.prototype.toString = function() {
  var str = "fn(" + this.args.join(", ") + ")";
  if (this.retval.types.length) str += " -> " + this.retval + ")";
  return str;
};

/*var Object_prototype = new Obj(null, props({
  toString: Fn([], _str),
  valueOf: Fn([], _num),
  hasOwnProperty: Fn([_str], _bool),
  propertyIsEnumerable: Fn([_str], _bool)
}));*/
