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
  type props = {
    content: string,
    imports: Ports.t,
    onLoad: (~exports: Ports.t, ~string: string=?) => unit,
    onChange: (string, ~exports: Ports.t) => unit,
  }

  let deserialise = (str: string) => {
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
    ret.contents
  }

  let make = props => {
    switch deserialise(props.content) {
    | Ok(s) => {
        React.useEffect(() => {
          // Run effects
          props.onLoad(~exports={Ports.facts: s, ruleStyle: None})
          None // or Some(() => {})
        }, [])

        <div
          className={"axiom-set axiom-set-"->String.concat(
            String.make(props.imports.ruleStyle->Option.getOr(Hybrid)),
          )}>
          {Dict.toArray(s)
          ->Array.mapWithIndex(((n, r), i) =>
            <RuleView
              rule={r}
              scope={[]}
              key={String.make(i)}
              style={props.imports.ruleStyle->Option.getOr(Hybrid)}>
              {React.string(n)}
              //<Name content={n} onChange={(_) => Error("BLAH!")} />
            </RuleView>
          )
          ->React.array}
        </div>
      }
    | Error(e) => {
        React.useEffect(() => {
          // Run effects
          props.onLoad(~exports={Ports.facts: Dict.make(), ruleStyle: None})
          None // or Some(() => {})
        }, [])
        <div className="error"> {React.string(e)} </div>
      }
    }
  }
}
