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

var p4 = Promise.resolve(Promise.resolve(10));
p4.then(function(value) {
  value; //: number
});

var arg5 = 1 < 2 ? Promise.resolve(10) : 20;
var p5 = Promise.resolve(arg5);
p5.then(function(value) {
  value; //: number
});

var p6 = Promise.resolve('t').then(function() {
  return 1 < 2 ? Promise.resolve(10) : 20;
}).then(function(value) {
  value; //: number
});

var p7 = Promise.resolve().then(function() {
  return 20;
}).then(function(value) {
  value; //: number
});

var p8 = Promise.resolve();
p8 //: Promise

function myResolve1(arg) {
  return Promise.resolve(arg);
}

myResolve1('s') //:: {:t: string}

function myResolve2(arg) {
  return Promise.resolve(arg);
}

myResolve2('s') //:: {:t: string|number}
myResolve2(4) //:: {:t: string|number}

myResolve2('s').then(function(value) {
  value; //: string|number
})

function myResolve3(arg7) {
  return Promise.resolve(arg7);
}

myResolve3(Promise.resolve(4)).then(function(value) {
  value; //: number
});

myResolve3(Promise.resolve(4)) //:: {:t: number}

myResolve3(4).then(function(value) {
  value; //: number
});
