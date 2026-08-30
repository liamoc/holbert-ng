module type TERM = {
  type t
  type schematic
  type meta
  type subst
  type gen
  let mapSubst: (subst, t => t) => subst
  let mergeSubsts: (subst, subst) => subst
  let substEqual: (subst, subst) => bool
  let prettyPrintSubst: (subst, ~scope: array<meta>) => string
  let substitute: (t, subst) => t
  let unify: (t, t, ~gen: gen=?) => Seq.t<subst>
  let makeSubst: unit => subst
  // law: unify(a,b) == [{}] iff equivalent(a,b)
  let equivalent: (t, t) => bool
  let substDeBruijn: (t, array<t>, ~from: int=?) => t
  let reduce: t => t
  let upshift: (t, int, ~from: int=?) => t
  let fresh: (gen, ~replacing: meta=?) => schematic
  let seen: (gen, schematic) => unit
  let place: (schematic, ~scope: array<meta>) => t
  let makeGen: unit => gen
  let parse: (string, ~scope: array<meta>, ~gen: gen=?) => result<(t, string), string>
  let parseMeta: string => result<(meta, string), string>
  let prettyPrint: (t, ~scope: array<meta>) => string
  let prettyPrintMeta: meta => string
  // will unifying t with a term give meaningful substitutions?
  let concrete: t => bool
}

module type JUDGMENT = {
  module Term: TERM
  type t
  let substitute: (t, Term.subst) => t
  let equivalent: (t, t) => bool
  let unify: (t, t, ~gen: Term.gen=?) => Seq.t<Term.subst>
  let substDeBruijn: (t, array<Term.t>, ~from: int=?) => t
  let reduce: t => t
  let upshift: (t, int, ~from: int=?) => t
  // Map a function over all terms in the judgment
  let mapTerms: (t, Term.t => Term.t) => t
  let parse: (string, ~scope: array<Term.meta>, ~gen: Term.gen=?) => result<(t, string), string>
  let prettyPrint: (t, ~scope: array<Term.meta>) => string
  let concrete: t => bool
}

module type REWRITABLE_JUDGMENT = {
  module Term: TERM
  include JUDGMENT with module Term := Term
  type path
  let asEquation: t => option<(Term.t, Term.t)>
  let locate: (t, path) => option<(Term.t, array<Term.meta>)>
  let replaceAt: (t, path, Term.t) => t
  let positions: t => array<(path, Term.t, array<Term.meta>)>
  let prettyPrintPath: path => string
  let parsePath: string => (path, string)
}

module type TERM_VIEW = {
  module Term: TERM
  type props = {term: Term.t, scope: array<Term.meta>}
  let make: props => React.element
  let makeMeta: Term.meta => React.element
  let makeEditableMeta: (Term.meta, ~onChange: Term.meta => unit) => React.element
  
}

module type JUDGMENT_VIEW = {
  module Term: TERM
  module Judgment: JUDGMENT with module Term := Term
  module TermView: TERM_VIEW with module Term := Term
  type props = {judgment: Judgment.t, scope: array<Term.meta>}
  let make: props => React.element
}
