var myIter = {
  [Symbol.asyncIterator]() {
    return {
      next() {
        return Promise.resolve({value: {a: 1, b: true}, done: false})
      }
    }
  }
}

async function run () {

  for await (var hello of myIter) {
    hello //:: {a: number, b: bool}
  }

  for await (var {a, b} of myIter) {
    a //: number
    b //: bool
  }

}
