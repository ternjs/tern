class Point2 {
  constructor(x, y) { this.x = x; this.y = y }
  plus(pt) {
    this //: Point2
    pt //: Point2
    return new Point2(this.x + pt.x, this.y + pt.y)
  }
  get xx() { return this.x }
  quux() { return 1 }
  static origin() { return new Point2(0, 0) }
}

class Point4 extends Point3 {
  constructor(x, y, z, u) { super(x, y, z); this.u = u }
  argh() { return 2 }
}

var Point3 = class extends Point2 {
  constructor(x, y, z) { super(x, y); this.z = z }
  foobar() { return true }
}

var p1 = new Point2(1, 2)
p1.x //: number
p1 //: Point2
var p2 = Point2.origin()
p2 //: Point2
p1.plus(p2) //: Point2
p1.foobar() //: ?
p1.xx //: number

var p3 = new Point3(0, 0, 5)
p3 //: Point3
p3.quux() //: number
p3.foobar() //: bool

var p4 = new Point4(1, 2, 3, 4)
p4 //: Point4
p4.argh() //: number
p4.foobar() //: bool
p4.quux() //: number
