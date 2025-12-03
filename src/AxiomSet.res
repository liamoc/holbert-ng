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
  type state = dict<Rule.t>
  type props = {
    content: state,
    imports: Ports.t,
    onChange: (state, ~exports: Ports.t=?) => unit,
  }

  let serialise = (state: state) => {
    state
    ->Dict.toArray
    ->Array.map(((k, r)) => r->Rule.prettyPrintTopLevel(~name=k))
    ->Array.join("\n")
  }
  let deserialise = (str: string, ~imports as _: Ports.t) => {
    let cur = ref(str)
    let go = ref(true)
    let results = Dict.make()
    let ret = ref(Error("impossible"))
    while go.contents {
      switch Rule.parseTopLevel(cur.contents, ~scope=[]) {
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
    ret.contents->Result.map(state => (state, {Ports.facts: state, ruleStyle: None}))
  }

  let make = props => {
    <div
      className={"axiom-set axiom-set-"->String.concat(
        String.make(props.imports.ruleStyle->Option.getOr(Hybrid)),
      )}
    >
      {Dict.toArray(props.content)
      ->Array.mapWithIndex(((n, r), i) =>
        <RuleView
          rule={r}
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
