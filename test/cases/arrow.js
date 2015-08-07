let f = (a, [b]) => ({a, b})

f(1, [true]) //:: {a: number, b: bool}

function wrap() {
  return () => this
}
wrap.call({a: 10}).call({b: true}) //:: {a: number}

function Obj() {
  this.x = true
}
Obj.prototype.map = function() {
  return [1, 2, 3].map(e => this.x)
}

;(new Obj).map() //: [bool]
