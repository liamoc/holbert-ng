open AtomDef

module StringA = MakeCoercible(
  StringA.Atom,
  {
    let coercions = [
      Coercion({
        tagEq: Symbolic.Atom.tagEq,
        coerce: s => Some([StringA.String(s)]),
      }),
      Coercion({
        tagEq: tagEqSExp,
        coerce: t =>
          switch t {
          | Var({idx}) => Some([StringA.Var({idx: idx})])
          | Schematic({schematic, allowed}) => Some([StringA.Schematic({schematic, allowed})])
          },
      }),
    ]
  },
)

module Symbolic = MakeCoercible(
  Symbolic.Atom,
  {
    let coercions = []
  },
)
