function buildCopy(o) {
  var oo = {};
  for (var prop in o) oo[prop] = o[prop];
  return oo;
}

// copy: {xx: number, yy: number}
var copy = buildCopy({xx: 10, yy: 20});
