open Signatures
module Make = (Term: TERM, Judgment : REWRITABLE_JUDGMENT with module Term := Term) => {
  module Method = Rewrite.Make(Term, Judgment)

  type props<'a> = {
    method: Method.t<'a>,
    scope: array<string>,
    ruleStyle: RuleView.style,
    gen: Term.gen,
    onChange: (Method.t<'a>, Term.subst) => unit,
  }

  type srProps<'a> = {
    "proof": 'a,
    "scope": array<string>,
    "ruleStyle": RuleView.style,
    "gen": Term.gen,
    "onChange": ('a, Term.subst) => unit,
  }
  let make = (subRender: srProps<'a> => React.element) =>
    props => {
      <div>
        <b> {React.string("rewrite ")} </b>
        <span className="proof-ruleName"> {React.string(props.method.ruleName)} </span>
        {
          if props.method.subgoals->Array.length > 0 { 
            <ul className="subgoals">
            {props.method.subgoals
            ->Array.mapWithIndex((sg, i) => {
              <li key={String.make(i)}>
                {React.createElement(
                  subRender,
                  {
                    "proof": sg,
                    "scope": props.scope,
                    "ruleStyle": props.ruleStyle,
                    "gen": props.gen,
                    "onChange": (newa, subst: Term.subst) =>
                      props.onChange(props.method->Method.updateAtKey(i, _ => newa), subst),
                  },
                )}
              </li>
            })
            ->React.array}
            </ul>
          } else {
            React.string("")
          }
        }
        <div className="proof-denest">
        {React.createElement(subRender,{
          "proof": props.method.newGoal,
          "scope": props.scope,
          "ruleStyle": props.ruleStyle,
          "gen": props.gen,
          "onChange": (newa, subst: Term.subst) => 
            props.onChange(props.method->Method.updateGoal(_=>newa), subst), 
        })}
        </div>
      </div>
    }
}