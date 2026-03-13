type stringSymbol = StringS(StringTerm.t) | ConstS(SExp.ConstSymbol.t)

module StringSymbol: SExpFunc.SYMBOL with type t = stringSymbol = {
  type t = stringSymbol
  type subst = Map.t<int, t>
  type gen = ref<int>
  let parse = (s, ~scope, ~gen: option<gen>=?) => {
    StringTerm.parse(s, ~scope, ~gen?)
    ->Result.map(((r, rest)) => (StringS(r), rest))
    ->Util.Result.or(() =>
      SExp.ConstSymbol.parse(s, ~scope, ~gen?)->Result.map(((r, rest)) => (ConstS(r), rest))
    )
  }
  let prettyPrint = (s, ~scope) =>
    switch s {
    | StringS(s) => StringTerm.prettyPrint(s, ~scope)
    | ConstS(s) => SExp.ConstSymbol.prettyPrint(s, ~scope)
    }
  let unify = (s1, s2) =>
    switch (s1, s2) {
    | (StringS(s1), StringS(s2)) =>
      StringTerm.unify(s1, s2)->Seq.map(subst => subst->Util.mapMapValues(v => StringS(v)))
    | (ConstS(s1), ConstS(s2)) =>
      SExp.ConstSymbol.unify(s1, s2)->Seq.map(subst => subst->Util.mapMapValues(v => ConstS(v)))
    | (_, _) => Seq.empty
    }
  let substitute = (s, subst: subst) =>
    switch s {
    | StringS(s) => {
        let stringSubs = subst->Util.mapMapValues(v =>
          switch v {
          | StringS(s) => s
          | _ => [StringTerm.Ghost]
          }
        )
        StringS(StringTerm.substitute(s, stringSubs))
      }
    | ConstS(s) => ConstS(s)
    }
  let lowerVar = idx => StringS([StringTerm.Var({idx: idx})])
  let lowerSchematic = (schematic, allowed) => StringS([StringTerm.Schematic({schematic, allowed})])
  let ghost = StringS([StringTerm.Ghost])
  let substDeBruijn = (s, substs: array<t>, ~from) =>
    switch s {
    | StringS(s) => {
        let stringSubs = substs->Array.map(v =>
          switch v {
          | StringS(s) => s
          | _ => [StringTerm.String("AYAYAYSLKDJFLSKDJ")]
          }
        )
        StringS(StringTerm.substDeBruijn(s, stringSubs, ~from))
      }
    | ConstS(s) => ConstS(s)
    }
  let constSymbol = s =>
    switch s {
    | StringS(_) => None
    | ConstS(s) => SExp.ConstSymbol.constSymbol(s)
    }
}

include SExpFunc.Make(StringSymbol)
