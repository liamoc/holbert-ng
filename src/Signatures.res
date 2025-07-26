module type TERM = {
  type t
  type schematic
  type meta
  type subst = Map.t<schematic, t>
  type gen
  let substitute: (t, subst) => t
  let unify: (t, t, ~gen: gen=?) => array<subst>
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
  let substitute: (t, Term.subst) => t
  let equivalent: (t, t) => bool
  let unify: (t, t, ~gen: Term.gen=?) => array<Term.subst>
  let substDeBruijn: (t, array<Term.t>, ~from: int=?) => t
  let upshift: (t, int, ~from: int=?) => t
  let parse: (string, ~scope: array<Term.meta>, ~gen: Term.gen=?) => result<(t, string), string>
  let prettyPrint: (t, ~scope: array<Term.meta>) => string
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
