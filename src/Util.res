let newline = "\n"
let mapMapValues = (m: Map.t<'a, 'b>, f: 'b => 'c) => {
  let nu = Map.make()
  m->Map.forEachWithKey((v, k) => {
    nu->Map.set(k, f(v))
  })
  nu
}

let mapIntersectionWith = (m1: Map.t<'a, 'b>, m2: Map.t<'a, 'b>, f: ('b, 'b) => 'c) => {
  let go = (m1, m2) => {
    let nu: Map.t<'a, 'c> = Map.make()
    m1->Map.forEachWithKey((v1, k) => {
      switch m2->Map.get(k) {
      | Some(v2) => nu->Map.set(k, f(v1, v2))
      | None => ()
      }
    })
    nu
  }
  if Map.size(m1) < Map.size(m2) {
    go(m1, m2)
  } else {
    go(m2, m1)
  }
}

let mapIntersection = (m1: Map.t<'a, 'b>, m2: Map.t<'a, 'b>) => {
  mapIntersectionWith(m1, m2, (a, b) => (a, b))
}

let withKey: ('props, int) => 'props = %raw(`(props, key) => ({...props, key})`)

let arrayWithIndex = (arr: array<React.element>) => {
  React.array(arr->Array.mapWithIndex((m, i) => <span key={String.make(i)}> m </span>))
}
@send external toString: 'a => string = "toString"
let showArray: array<'a> => string = a => `[${Array.toString(a)}]`
let showTuple: (('a, 'b)) => string = ((a, b)) => `(${toString(a)} ${toString(b)})`
