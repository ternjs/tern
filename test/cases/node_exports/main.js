// plugin=node

module.exports = f.f1 = f.f2 = f;

function f() {}

f.f3 = function() {};

module.exports.x = require("./exports").x;

//node_exports: (reassigned to func), x, f1, f2, f3
