
type props<'a> = {
  method: SHORewrite.t<'a>,
  scope: array<string>,
  ruleStyle: RuleView.style,
  gen: SHOTerm.gen,
  onChange: (SHORewrite.t<'a>, SHOTerm.subst) => unit,
}
module Method = SHORewrite
type srProps<'a> = {
  "proof": 'a,
  "scope": array<string>,
  "ruleStyle": RuleView.style,
  "gen": SHOTerm.gen,
  "onChange": ('a, SHOTerm.subst) => unit,
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
                  "onChange": (newa, subst: SHOTerm.subst) =>
                    props.onChange(props.method->SHORewrite.updateAtKey(i, _ => newa), subst),
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
        "onChange": (newa, subst: SHOTerm.subst) => 
          props.onChange(props.method->SHORewrite.updateGoal(_=>newa), subst), 
      })}
      </div>
    </div>
  }
