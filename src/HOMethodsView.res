module ConstructorDisjointnessView = {
  module Method = HOMethods.ConstructorDisjointness

  type props<'a> = {
    method: Method.t<'a>,
    scope: array<string>,
    ruleStyle: RuleView.style,
    gen: HOTerm.gen,
    onChange: (Method.t<'a>, HOTerm.subst) => unit,
  }

  type srProps<'a> = {
    "proof": 'a,
    "scope": array<string>,
    "ruleStyle": RuleView.style,
    "gen": HOTerm.gen,
    "onChange": ('a, HOTerm.subst) => unit,
  }
  let make = (subRender: srProps<'a> => React.element) =>
    props => {
      <div>
        <b> {React.string("disjointness ")} </b>
        <span className="proof-ruleName"> {React.string(props.method.factName)} </span>
      </div>
    }
}

module ConstructorInjectivityView = {
  module Method = HOMethods.ConstructorInjectivity

  type props<'a> = {
    method: Method.t<'a>,
    scope: array<string>,
    ruleStyle: RuleView.style,
    gen: HOTerm.gen,
    onChange: (Method.t<'a>, HOTerm.subst) => unit,
  }

  type srProps<'a> = {
    "proof": 'a,
    "scope": array<string>,
    "ruleStyle": RuleView.style,
    "gen": HOTerm.gen,
    "onChange": ('a, HOTerm.subst) => unit,
  }
  let make = (subRender: srProps<'a> => React.element) =>
    props => {
      <div>
        <b> {React.string("injectivity ")} </b>
        <span className="proof-ruleName"> {React.string(props.method.factName)} </span>
        <div className="proof-denest">
        {React.createElement(subRender,{
          "proof": props.method.subgoal,
          "scope": props.scope,
          "ruleStyle": props.ruleStyle,
          "gen": props.gen,
          "onChange": (newa, subst: HOTerm.subst) => 
            props.onChange(props.method->Method.updateGoal(_=>newa), subst), 
        })}
        </div>      
      </div>
    }
}