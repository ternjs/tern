let map = new Map

map.set(55, "hello")
map.get(55) //: string

for (let val of map.values())
  val //: string

for (let key of map.keys())
  key //: number

for (let pair of map)
  pair //: [string]

map.forEach(function(val, key) {
  val //: string
  key //: number
})
