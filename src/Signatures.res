module type TERM = {
  type t
  type schematic
  type meta
  type subst
  type gen
  let mapSubst: (subst, t => t) => subst
  let substEqual: (subst, subst) => bool
  let prettyPrintSubst: (subst, ~scope: array<meta>) => string
  let substitute: (t, subst) => t
  let unify: (t, t, ~gen: gen=?) => Seq.t<subst>
  // law: unify(a,b) == [{}] iff equivalent(a,b)
  let equivalent: (t, t) => bool
  let substDeBruijn: (t, array<t>, ~from: int=?) => t
  let upshift: (t, int, ~from: int=?) => t
  let fresh: (gen, ~replacing: meta=?) => schematic
  let seen: (gen, schematic) => unit
  let place: (schematic, ~scope: array<meta>) => t
  let makeGen: unit => gen
  let parse: (string, ~scope: array<meta>, ~gen: gen=?) => result<(t, string), string>
  let parseMeta: string => result<(meta, string), string>
  let prettyPrint: (t, ~scope: array<meta>) => string
  let prettyPrintMeta: meta => string
}

module type JUDGMENT = {
  module Term: TERM
  type t
  type subst
  type substVal
  let mapSubst: (subst, substVal => substVal) => subst
  let substitute: (t, subst) => t
  let equivalent: (t, t) => bool
  let unify: (t, t, ~gen: Term.gen=?) => Seq.t<subst>
  let substDeBruijn: (t, array<substVal>, ~from: int=?) => t
  let upshift: (t, int, ~from: int=?) => t
  let upshiftSubstVal: (substVal, int, ~from: int=?) => substVal
  let parse: (string, ~scope: array<Term.meta>, ~gen: Term.gen=?) => result<(t, string), string>
  let parseSubstVal: (
    string,
    ~scope: array<Term.meta>,
    ~gen: Term.gen=?,
  ) => result<(substVal, string), string>
  let prettyPrint: (t, ~scope: array<Term.meta>) => string
  let prettyPrintSubstVal: (substVal, ~scope: array<Term.meta>) => string
}

module type TERM_VIEW = {
  module Term: TERM
  type props = {term: Term.t, scope: array<Term.meta>}
  let make: props => React.element
  let makeMeta: Term.meta => React.element
}

module type JUDGMENT_VIEW = {
  module Term: TERM
  module Judgment: JUDGMENT with module Term := Term
  module TermView: TERM_VIEW with module Term := Term
  type props = {judgment: Judgment.t, scope: array<Term.meta>}
  let make: props => React.element
}
