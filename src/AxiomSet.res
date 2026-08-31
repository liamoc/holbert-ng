open Signatures
open Component
module Make = (
  Term: TERM,
  Judgment: JUDGMENT with module Term := Term,
  JudgmentView: JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment,
) => {
  module Rule = Rule.Make(Term, Judgment)
  module RuleView = RuleView.Make(Term, Judgment, JudgmentView)
  module Ports = Ports(Term, Judgment)
  type state = { rules: dict<Rule.t>, grammar: Term.grammar }
  type props = {
    content: state,
    imports: Ports.t,
    onChange: (state, ~exports: Ports.t=?) => unit,
    reset: unit => unit,
  }

  let serialise = (state: state) => {
    state.rules
    ->Dict.toArray
    ->Array.map(((k, r)) => r->Rule.prettyPrintTopLevel(~name=k, ~grammar=state.grammar))
    ->Array.join("\n")
  }
  let deserialise = (str: string, ~imports: Ports.t) => {
    let cur = ref(str)
    let go = ref(true)
    let results = Dict.make()
    let ret = ref(Error("impossible"))
    while go.contents {
      switch Rule.parseTopLevel(cur.contents, ~grammar=imports.grammar, ~scope=[]) {
      | Ok((t, n), rest) =>
        if n->String.trim == "" {
          go := false
          ret := Error("Rule given with no name")
        } else {
          Dict.set(results, n, t)
          if rest->String.trim == "" {
            go := false
            ret := Ok(results)
          } else {
            cur := rest
          }
        }
      | Error(e) => {
          go := false
          ret := Error(e)
        }
      }
    }
    ret.contents->Result.map(state => (
      {rules: state, grammar: imports.grammar }, 
      {Ports.facts: state, ruleStyle: None, grammar: Term.emptyGrammar}))
  }

  let make = props => {
    <div
      className={"axiom-set axiom-set-"->String.concat(
        String.make(props.imports.ruleStyle->Option.getOr(Hybrid)),
      )}
    >
      {Dict.toArray(props.content.rules)
      ->Array.mapWithIndex(((n, r), i) =>
        <RuleView
          rule={r}
          grammar={props.content.grammar}
          scope={[]}
          key={String.make(i)}
          style={props.imports.ruleStyle->Option.getOr(Hybrid)}
        >
          {React.string(n)}
        </RuleView>
      )
      ->React.array}
    </div>
  }
}
