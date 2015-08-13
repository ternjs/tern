var p = new Promise(function(accept, reject) {
  reject; //: fn(reason: ?)
  accept({x: 20});
});

p.then(function(value) {
  value; //:: {x: number}
}).then(function(value) {
  value; //:: {x: number}
});

var p2 = new Promise(function(acc) { acc("hi"); });

Promise.all([p2]).then(function(value) {
  value; //: [string]
  return Promise.resolve(33);
}).then(function(value) {
  value; //: number
});

var p3 = Promise.resolve(10);

p3.then(function(value) {
  value; //: number
  return true;
}).then(function(value) {
  value; //: bool
});
