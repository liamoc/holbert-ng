open Signatures
module Make = (Term: TERM, Judgment : REWRITABLE_JUDGMENT with module Term := Term) => {
  module Context = Method.Context(Term, Judgment)
  module Method = Rewrite.Make(Term, Judgment)

  type props<'a> = {
    method: Method.t<'a>,
    ctx: Context.t,
    ruleStyle: RuleView.style,
    gen: Term.gen,
    onChange: (Method.t<'a>, Term.subst) => unit,
  }

  type srProps<'a> = {
    "proof": 'a,
    "ctx": Context.t,
    "ruleStyle": RuleView.style,
    "gen": Term.gen,
    "onChange": ('a, Term.subst) => unit,
  }
  let make = (subRender: srProps<'a> => React.element) =>
    props => {
      <div>
        <b> {React.string("rewrite ")} </b>
        <MethodView.RuleRefView ruleRef=props.method.ruleName assms=props.ctx.localFactNames />
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
                    "ctx": props.ctx,
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
          "ctx": props.ctx,
          "ruleStyle": props.ruleStyle,
          "gen": props.gen,
          "onChange": (newa, subst: Term.subst) => 
            props.onChange(props.method->Method.updateGoal(_=>newa), subst), 
        })}
        </div>
      </div>
    }
}