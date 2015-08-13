var myIter = {
  [Symbol.iterator]() {
    return {
      next() {
        return {value: {a: 1, b: true}, done: false}
      }
    }
  }
}

for (var hello of myIter) {
  hello //:: {a: number, b: bool}
}

for (var {a, b} of myIter) {
  a //: number
  b //: bool
}

for (let elt of [1, 2, 3])
  elt //: number
