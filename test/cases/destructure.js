let [a] = [1, 2]
a //: number

let {c, d} = {c: "hello", d: true}
c //: string
d //: bool

function foo([e], {f=1}, {g}, ...h) {
  e //: bool
  f //: number
  g //: string
  h //: [number]
}
// Check for argument leakage
e //: ?
g //: ?

foo([false], blah(), {g: "hello"}, 20)

let i, j
;({i, j, k: [l, ...m]} = {i: 1, j: false, k: ["a", "b"]})

i //: number
j //: bool
l //: string
m //: [string]

var out = {}
;[out.prop, out.prop2] = [55, true]
out //:: {prop2: bool, prop: number}

var [n, o] = ["a", false]
n //: string
o //: bool
