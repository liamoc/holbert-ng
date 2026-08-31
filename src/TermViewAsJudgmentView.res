module Make = (
  Term: Signatures.TERM,
  Judgment: Signatures.JUDGMENT with module Term := Term and type t = Term.t,
  TermView: Signatures.TERM_VIEW with module Term := Term,
): (Signatures.JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment) => {
  module TermView = TermView
  type props = {
    judgment: Judgment.t,
    grammar: Term.grammar,
    scope: array<Term.meta>,
  }
  let make = ({judgment, scope, grammar}) => TermView.make({term: judgment, scope, grammar})
}
