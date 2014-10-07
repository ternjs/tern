var util = require("./util");

var infer = require("../lib/infer");
var def = require("../lib/def").init({}, infer);

exports.runTests = function(filter) {
  var cx = new infer.Context([],null);
  infer.withContext(cx, function() {
    cx.protos.Object = new infer.Obj(null, "Object.prototype");
    cx.topScope = new infer.Scope();
    cx.topScope.name = "<top>";
    cx.protos.Array = new infer.Obj(true, "Array.prototype");
    cx.protos.Function = new infer.Obj(true, "Function.prototype");
    cx.protos.RegExp = new infer.Obj(true, "RegExp.prototype");
    cx.protos.String = new infer.Obj(true, "String.prototype");
    cx.protos.Number = new infer.Obj(true, "Number.prototype");
    cx.protos.Boolean = new infer.Obj(true, "Boolean.prototype");
    cx.str = new infer.Prim(cx.protos.String, "string");
    cx.bool = new infer.Prim(cx.protos.Boolean, "bool");
    cx.num = new infer.Prim(cx.protos.Number, "number");
    cx.curOrigin = null;

    var typeString = "(number|string)";
    var unionType = new def.TypeParser(typeString, "", null, true).parseType("??", true);
    util.addTest();
    util.addFile();
    if (unionType.toString() !== "number,string") util.failure("parsing " + typeString + " failed, got: " + unionType.toString());
  });

}