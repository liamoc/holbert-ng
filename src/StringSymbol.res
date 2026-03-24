type t = StringS(StringA.Atom.t) | ConstS(string)

module Atom: SExpFunc.ATOM with type t = t = {
  type t = t
  type subst = Map.t<int, t>
  type gen = ref<int>
  let parse = (s, ~scope, ~gen: option<gen>=?) => {
    StringA.Atom.parse(s, ~scope, ~gen?)
    ->Result.map(((r, rest)) => (StringS(r), rest))
    ->Util.Result.or(() =>
      Symbolic.Atom.parse(s, ~scope, ~gen?)->Result.map(((r, rest)) => (ConstS(r), rest))
    )
  }
  let prettyPrint = (s, ~scope) =>
    switch s {
    | StringS(s) => StringA.Atom.prettyPrint(s, ~scope)
    | ConstS(s) => Symbolic.Atom.prettyPrint(s, ~scope)
    }
  let unify = (s1, s2, ~gen=?) =>
    switch (s1, s2) {
    | (StringS(s1), StringS(s2)) =>
      StringA.Atom.unify(s1, s2, ~gen?)->Seq.map(subst => subst->Util.mapMapValues(v => StringS(v)))
    | (ConstS(s1), ConstS(s2)) =>
      Symbolic.Atom.unify(s1, s2, ~gen?)->Seq.map(subst => subst->Util.mapMapValues(v => ConstS(v)))
    | (_, _) => Seq.empty
    }
  let substitute = (s, subst: subst) =>
    switch s {
    | StringS(s) => {
        let stringSubs =
          subst
          ->Map.entries
          ->Iterator.toArrayWithMapper(((i, v)) =>
            switch v {
            | StringS(s) => Some((i, s))
            | _ => None
            }
          )
          ->Array.keepSome
          ->Map.fromArray
        StringS(StringA.Atom.substitute(s, stringSubs))
      }
    | ConstS(s) => ConstS(s)
    }
  let upshift = (s, amount: int, ~from=?) =>
    switch s {
    | StringS(s) => StringS(s->StringA.Atom.upshift(amount, ~from?))
    | ConstS(s) => ConstS(s)
    }
  let lowerVar = idx => Some(StringS([StringA.Var({idx: idx})]))
  let lowerSchematic = (schematic, allowed) => Some(
    StringS([StringA.Schematic({schematic, allowed})]),
  )
  let substDeBruijn = (s, substs: array<option<t>>, ~from=?) =>
    switch s {
    | StringS(s) => {
        let stringSubs = substs->Array.map(s =>
          switch s {
          | Some(StringS(s)) => Some(s)
          | _ => None
          }
        )
        StringS(StringA.Atom.substDeBruijn(s, stringSubs, ~from?))
      }
    | ConstS(s) => ConstS(s)
    }
  let concrete = s =>
    switch s {
    | StringS(s) => StringA.Atom.concrete(s)
    | ConstS(_) => false
    }
}

module AtomView: SExpViewFunc.ATOM_VIEW with module Atom := Atom = {
  type props = {name: Atom.t, scope: array<string>}
  let make = ({name, scope}: props) =>
    switch name {
    | StringS(name) => <StringA.AtomView name scope />
    | ConstS(name) => <Symbolic.AtomView name scope />
    }
}
