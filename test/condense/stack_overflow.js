exports.parseCertString = function parseCertString() {
  var out = {};
  // isolated from https://github.com/joyent/node/blob/5106cadffba559ac788a8c1b9a555a6d192d95aa/lib/tls.js#L200
  out[3] = [out[3]];
  return out;
};
