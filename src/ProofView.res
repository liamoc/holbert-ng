open Signatures
open MethodView

module Make = (
  Term: TERM,
  Judgment: JUDGMENT with module Term := Term,
  JudgmentView: JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment,
  MethodView: METHOD_VIEW with module Term := Term and module Judgment := Judgment,
) => {
  module Rule = Rule.Make(Term, Judgment)
  module ScopeView = ScopeView.Make(Term, JudgmentView.TermView)
  module Proof = Proof.Make(Term, Judgment, MethodView.Method)

  type props = {
    proof: Proof.checked,
    scope: array<Term.meta>,
    ruleStyle: RuleView.style,
    gen: Term.gen,
    onChange: (Proof.checked, Judgment.subst) => unit,
  }
  module RuleView = RuleView.Make(Term, Judgment, JudgmentView)
  @react.componentWithProps
  let rec make = (props: props) => {
    switch props.proof {
    | Proof.Checked({fixes, assumptions, method, rule}) => {
        let scope = Array.concat(fixes, props.scope)
        <>
          <ScopeView scope=fixes />
          <ul className="proof-assumptions">
            {Belt.Array.zipBy(assumptions, rule.premises, (n, r) => {
              <li key={n}>
                <RuleView rule=r style=props.ruleStyle scope> {React.string(n)} </RuleView>
              </li>
            })->React.array}
          </ul>
          <div className="proof-show">
            <JudgmentView judgment={rule.conclusion} scope />
            {switch method {
            | Goal(options) =>
              options(props.gen)
              ->Dict.toArray
              ->Array.map(((str, (opt, subst))) => {
                <button
                  key=str
                  onClick={_ =>
                    props.onChange(
                      Proof.Checked({fixes, assumptions, method: Do(opt), rule}),
                      subst,
                    )}
                >
                  {React.string(str)}
                </button>
              })
              ->React.array
            | Do(method) =>
              React.createElement(
                MethodView.make(p =>
                  make({
                    proof: p["proof"],
                    scope: p["scope"],
                    ruleStyle: p["ruleStyle"],
                    gen: p["gen"],
                    onChange: p["onChange"],
                  })
                ),
                {
                  method,
                  scope,
                  ruleStyle: props.ruleStyle,
                  gen: props.gen,
                  onChange: (newm, subst) => {
                    props.onChange(
                      Proof.Checked({fixes, assumptions, method: Do(newm), rule}),
                      subst,
                    )
                  },
                },
              )
            }}
          </div>
        </>
      }
    | Proof.ProofError({raw: _, rule: _, msg}) => <div className="error"> {React.string(msg)} </div>
    }
  }
}
