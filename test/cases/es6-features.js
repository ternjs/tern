class Foo {
  constructor(a = 10, ...b) {
    this.a = a
    this.b = b
  }

  method() { return this.a + this.b }

  get x() { return `template${this.a}` }
}

let x = 1

const y = {
  func() { return 10 },
  get b() { return false },
  ["foo" + "bar"]: 800
}

let it = function*(n) {
  for (let i = 0; i < n; i++) yield n
}

console.log([for (a of [1, 2, ...it(10)]) a * 2])

let [a, b] = [1, 2];

let x = ([a], {b}) => a + b

let [e1,,e3] = ["5", false, 6]
