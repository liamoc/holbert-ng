module ConstructorDisjointnessView = {
  module Method = HOMethods.ConstructorDisjointness

  type props<'a> = {
    method: Method.t<'a>,
    scope: array<string>,
    assms: array<string>,
    ruleStyle: RuleView.style,
    gen: HOTerm.gen,
    onChange: (Method.t<'a>, HOTerm.subst) => unit,
  }

  type srProps<'a> = {
    "proof": 'a,
    "scope": array<string>,
    "assms": array<string>,
    "ruleStyle": RuleView.style,
    "gen": HOTerm.gen,
    "onChange": ('a, HOTerm.subst) => unit,
  }
  let make = (subRender: srProps<'a> => React.element) =>
    props => {
      <div>
        <b> {React.string("disjointness ")} </b>
        <MethodView.RuleRefView ruleRef=props.method.factName assms=props.assms />
      </div>
    }
}

module ConstructorInjectivityView = {
  module Method = HOMethods.ConstructorInjectivity

  type props<'a> = {
    method: Method.t<'a>,
    scope: array<string>,
    assms: array<string>,
    ruleStyle: RuleView.style,
    gen: HOTerm.gen,
    onChange: (Method.t<'a>, HOTerm.subst) => unit,
  }

  type srProps<'a> = {
    "proof": 'a,
    "scope": array<string>,
    "assms": array<string>,
    "ruleStyle": RuleView.style,
    "gen": HOTerm.gen,
    "onChange": ('a, HOTerm.subst) => unit,
  }
  let make = (subRender: srProps<'a> => React.element) =>
    props => {
      <div>
        <b> {React.string("injectivity ")} </b>
        <MethodView.RuleRefView ruleRef=props.method.factName assms=props.assms />
        <div className="proof-denest">
        {React.createElement(subRender,{
          "proof": props.method.subgoal,
          "scope": props.scope,
          "assms": props.assms,
          "ruleStyle": props.ruleStyle,
          "gen": props.gen,
          "onChange": (newa, subst: HOTerm.subst) => 
            props.onChange(props.method->Method.updateGoal(_=>newa), subst), 
        })}
        </div>      
      </div>
    }
}