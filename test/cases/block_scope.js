function x(a) {
  {
    let a = 4
    let b = 10
    var c = true
    a; //: number
    b; //: number
    c; //: bool
  }
  a; //: string
  b; //: ?
  c; //: bool
}

x("hello")
