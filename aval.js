// Abstract types

// FIXME remove this, use direct assignments
var hop = Object.prototype.hasOwnProperty;
function add(obj, props) {
  for (var p in props) if (hop.call(props, p)) obj[p] = props[p];
  return obj;
}

var flag_recGuard = 1, flag_speculative = 2, flag_initializer = 4;

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
PropIsSubset.prototype.propHint = function() { return this.prop; };

var PropHasSubset = exports.PropHasSubset = function(prop, target) {
  this.target = target; this.prop = prop;
}
PropHasSubset.prototype.addType = function(type) {
  if (type.ensureProp)
    this.target.propagate(type.ensureProp(this.prop));
};
PropHasSubset.prototype.propHint = function() { return this.prop; };

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
IsCallee.prototype.typeHint = function(maxDepth) {
  return new Fn(null, this.self, this.args, this.retval).toString(maxDepth);
};

function IsProto(name) { this.name = name; }
IsProto.prototype.addType = function(o) {
  if (o instanceof Obj && !o.name) o.name = this.name;
};
IsProto.prototype.typeHint = function() { return this.name; };

var IsAdded = exports.IsAdded = function(other, target) {
  this.other = other; this.target = target;
}
IsAdded.prototype.addType = function(type) {
  if (type == _str)
    this.target.addType(_str);
  else if (type == _num && hasType(_num, this.other))
    this.target.addType(_num);
};
IsAdded.prototype.typeHint = function(maxDepth) {
  return this.other.toString(maxDepth);
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

var Obj = exports.Obj = function(name) {
  this.props = Object.create(null);
  this.name = name;
  cx.objTypes.push(this);
};
add(Obj.prototype, {
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
});

function compatible(one, two) {
  if (two instanceof AVal) {
    return !one.types.length || !two.types.length || one.isDominant(two.dominantType());
  } else {
    return one.isDominant(two);
  }
}

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

var Fn = exports.Fn = function(name, self, args, retval) {
  Obj.call(this, name);
  this.self = self;
  this.args = args;
  this.retval = retval;
};
Fn.prototype = add(Object.create(Obj.prototype), {
  constructor: Fn,
  toString: function(maxDepth) {
    if (maxDepth) maxDepth--;
    var str = "fn(" + this.args.map(function(x) { return x.toString(maxDepth); }).join(", ") + ")";
    var rettype = this.retval.toString(maxDepth);
    if (rettype != "<?>") str += " -> " + rettype;
    return str;
  },
  ensureProp: function(prop, speculative) {
    var newProto = this.name && prop == "prototype" && !("prototype" in this.props);
    var retval = Obj.prototype.ensureProp.call(this, prop, speculative);
    if (newProto) retval.propagate(new IsProto(this.name));
    return retval;
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
