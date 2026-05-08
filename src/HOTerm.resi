module type S = {
  module Atom: AtomDef.ATOM_CHOICE
  type rec t =
    | Symbol({name: Atom.t, constructor: bool})
    | Var({idx: int})
    | Schematic({schematic: int})
    | Lam({name: string, body: t})
    | App({func: t, arg: t})
    | Unallowed

  include Signatures.TERM
    with type t := t
    and type meta = string
    and type schematic = int
    and type subst = Belt.Map.Int.t<t>

  let emptySubst: subst
  let strip: t => (t, array<t>)
  let app: (t, array<t>) => t
  let mkvars: int => array<t>
  let mapTerms: (t, t => t) => t
  exception UnifyFail(string)
  let substAdd: (subst, schematic, t) => subst
  let unifyTerm: (t, t, subst, ~gen: option<gen>) => Seq.t<subst>
  let reduceSubst: subst => subst
  let rewrite: (t, t, t, ~subst: subst, ~gen: option<gen>) => (subst, t)
  let isEqualityAtom: Atom.t => bool
}

module Make: (Atom: AtomDef.ATOM_CHOICE) => (S with module Atom = Atom)
