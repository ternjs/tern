async function scope () {
  async function * myGen () {
    yield {c: 1};
    return {c: 2};
  }
  
  var iter = myGen();
  
  for await (const item of iter) {
    item; //:: {c: number}
  }
}