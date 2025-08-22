let newline = "\n"
let mapMapValues = (m: Map.t<'a, 'b>, f: 'b => 'c) => {
  let nu = Map.make()
  m->Map.forEachWithKey((v, k) => {
    nu->Map.set(k, f(v))
  })
  nu
}

@send external toString: 'a => string = "toString"
let showArray: array<'a> => string = a => `[${Array.toString(a)}]`
let showTuple: (('a, 'b)) => string = ((a, b)) => `(${toString(a)} ${toString(b)})`

let prettyPrintMap = (
  m: Map.t<'k, 'v>,
  ~showK: 'k => string=toString,
  ~showV: 'v => string=toString,
) => {
  m
  ->Map.entries
  ->Iterator.toArray
  ->Array.map(((k, v)) => {
    (showK(k), showV(v))
  })
  ->showArray
}

let prettyPrintIntMap = (m: Belt.Map.Int.t<'v>, ~showV: 'v => string=toString) => {
  m
  ->Belt.Map.Int.toArray
  ->Array.map(((k, v)) => {
    (Int.toString(k), showV(v))
  })
  ->showArray
}

let mapIntersectionWith = (m1: Map.t<'k, 'a>, m2: Map.t<'k, 'b>, f: ('a, 'b) => 'c) => {
  let go = (m1, m2) => {
    let nu: Map.t<'k, 'c> = Map.make()
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

let mapUnionWith = (m1: Map.t<'k, 'a>, m2: Map.t<'k, 'a>, f: ('a, 'a) => 'a) => {
  let nu = Map.make()
  m1->Map.forEachWithKey((v1, k) => {
    switch m2->Map.get(k) {
    | Some(v2) => nu->Map.set(k, f(v1, v2))
    | None => nu->Map.set(k, v1)
    }
  })
  m2->Map.forEachWithKey((v, k) => {
    switch nu->Map.get(k) {
    | Some(_) => ()
    | None => nu->Map.set(k, v)
    }
  })
  nu
}

// left biased
let mapUnion = (m1, m2) => mapUnionWith(m1, m2, (v1, _v2) => v1)

let mapIntersection = (m1: Map.t<'k, 'a>, m2: Map.t<'k, 'b>): Map.t<'k, ('a, 'b)> =>
  mapIntersectionWith(m1, m2, (b, c) => (b, c))

let withKey: ('props, int) => 'props = %raw(`(props, key) => ({...props, key})`)

let mapEqual = (m1, m2) => {
  Map.size(m1) == Map.size(m1) &&
    mapIntersection(m1, m2)
    ->Map.values
    ->Iterator.toArray
    ->Array.filter(((a, b)) => a == b)
    ->Array.length == Map.size(m2)
}

let arrayWithIndex = (arr: array<React.element>) => {
  React.array(arr->Array.mapWithIndex((m, i) => <span key={String.make(i)}> m </span>))
}
let execRe = (re, str) => {
  re
  ->RegExp.exec(str)
  ->Option.map(result => {
    open RegExp.Result
    (matches(result), fullMatch(result)->String.length)
  })
}

let identRegexStr = `([a-zA-Z][a-zA-Z\\d]*)`

let intersperse = (a: array<'a>, ~with: 'a) =>
  a->Array.flatMapWithIndex((e, i) =>
    if i == 0 {
      [e]
    } else {
      [with, e]
    }
  )

exception Unreachable(string)
exception Err(string)
