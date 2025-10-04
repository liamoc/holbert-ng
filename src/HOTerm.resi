type rec t =
  | Symbol({name: string})
  | Var({idx: int})
  | Schematic({schematic: int})
  | Lam({name: string, body: t})
  | App({func: t, arg: t})
  // Unallowed is used internally in unify, where Nipkow 1993 uses Var(-infinity)
  | Unallowed

include Signatures.TERM
  with type t := t
  and type meta = string
  and type schematic = int
  and type subst = Belt.Map.Int.t<t>

let emptySubst: subst
let strip: t => (t, array<t>)
let app: (t, array<t>) => t
// exposed for testing purposes
exception UnifyFail(string)
let substAdd: (subst, schematic, t) => subst
let unifyTerm: (t, t, subst, ~gen: option<gen>) => subst
let reduceSubst: subst => subst
