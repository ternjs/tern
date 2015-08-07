`foo` //: string
var x
`foo${x = 10}bar` //: string
x //: number

function build(strs, a, b) {
  strs //: [string]
  a //: number
  b //: string
  return true
}

build`foo${10}bar${'hi'}` //: bool
