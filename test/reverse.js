// Test inverted flow of information (against propagation) when no
// accurate information is available.

var x = {foo: 1, bar: 2};

function num(x) {
  return x * 3;
}

function obj(x) {
  return x.foo - x.bar;
}

// num: fn(<number>) -> <number>
// obj: fn({bar: <number>, foo: <number>}) -> <number>
