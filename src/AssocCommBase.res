module type AC_CONST = {
  let op: (int, int) => int
  let opInv: (int, int) => option<int>
  let opString: string
  let openTerm: string
  let closeTerm: string
  let formatMultiple: (string, int) => string
  let formatMultipleConst: int => string
  let parseConst: Parser.t<int>
  let parseVar: (~scope: array<string>) => Parser.t<int>
  let parseSchema: (~gen: ref<int>=?, ~scope: array<string>) => Parser.t<(int, array<int>)>
}

module type AC_BASE_ATOM = {
  module Const: AC_CONST
  type base = {
    schemas: Belt.Map.Int.t<{allowed: array<int>, count: int}>,
    vars: Belt.Map.Int.t<int>,
    const: int,
  }
  let identity: base
  let const: int => base
  let var: int => base
  let schematic: (int, array<int>) => base
  include module type of AtomBase.Make({
    type t = base
  })
}

module Make = (Const: AC_CONST): AC_BASE_ATOM => {
  module Const = Const
  type base = {
    schemas: Belt.Map.Int.t<{allowed: array<int>, count: int}>,
    vars: Belt.Map.Int.t<int>,
    const: int,
  }
  module IntMap = Belt.Map.Int
  let identity = {schemas: IntMap.empty, vars: IntMap.empty, const: 0}
  let const = n => {...identity, const: n}
  let var = idx => {...identity, vars: IntMap.empty->IntMap.set(idx, 1)}
  let schematic = (schematic, allowed) => {
    ...identity,
    schemas: IntMap.empty->IntMap.set(schematic, {allowed, count: 1}),
  }
  include AtomBase.Make({type t = base})
}

module Nat = Make({
  let op = (a, b) => a + b
  let opInv = (a, b) =>
    if a - b < 0 {
      None
    } else {
      Some(a - b)
    }
  let opString: string = "+"
  let openTerm: string = "nat("
  let closeTerm: string = ")"
  let formatMultiple: (string, int) => string = (s, n) => `${n->Int.toString}${s}`
  let formatMultipleConst: int => string = n => Int.toString(n)
  let parseConst: Parser.t<int> = Parser.decimal
  let parseVar = (~scope) => Parser.Util.varIdx(~scope)
  let seen = (g: ref<int>, s: int) => {
    if s >= g.contents {
      g := s + 1
    }
  }
  let parseSchema = (~gen=?, ~scope: array<string>): Parser.t<(int, array<int>)> => {
    Parser.Util.schemaLit(~scope)->Parser.map(((schematic, allowed)) => {
      gen->Option.map(g => allowed->Array.forEach(n => seen(g, n)))->ignore
      (schematic, allowed)
    })
  }
})
