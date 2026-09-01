module Context = Method.Context(HOTerm,HOTerm)
module ConstructorDisjointnessView = {
  module Method = HOMethods.ConstructorDisjointness

  type props<'a> = {
    method: Method.t<'a>,
    ctx: Context.t,
    ruleStyle: RuleView.style,
    grammar: HOTerm.grammar,    
    gen: HOTerm.gen,
    onChange: (Method.t<'a>, HOTerm.subst) => unit,
  }

  type srProps<'a> = {
    "proof": 'a,
    "ctx": Context.t,
    "ruleStyle": RuleView.style,
    "grammar": HOTerm.grammar,    
    "gen": HOTerm.gen,
    "onChange": ('a, HOTerm.subst) => unit,
  }
  
  let summary = props => <span>{React.string("disjointness")}</span>  
  let make = (_subRender: srProps<'a> => React.element) =>
    props => {
      <div>
        <b> {React.string("disjointness ")} </b>
        <MethodView.RuleRefView ruleRef=props.method.factName assms=props.ctx.localFactNames />
      </div>
    }
}

module ConstructorInjectivityView = {
  module Method = HOMethods.ConstructorInjectivity

  type props<'a> = {
    method: Method.t<'a>,
    ctx: Context.t,
    ruleStyle: RuleView.style,
    grammar: HOTerm.grammar,
    gen: HOTerm.gen,
    onChange: (Method.t<'a>, HOTerm.subst) => unit,
  }

  type srProps<'a> = {
    "proof": 'a,
    "ctx": Context.t,
    "ruleStyle": RuleView.style,
    "grammar": HOTerm.grammar,
    "gen": HOTerm.gen,
    "onChange": ('a, HOTerm.subst) => unit,
  }
  let summary = props => <span>{React.string("injectivity")}</span>
  let make = (subRender: srProps<'a> => React.element) =>
    props => {
      <div>
        <b> {React.string("injectivity ")} </b>
        <MethodView.RuleRefView ruleRef=props.method.factName assms=props.ctx.localFactNames />
        <div className="proof-denest">
        {React.createElement(subRender,{
          "proof": props.method.subgoal,
          "ctx": props.ctx,
          "ruleStyle": props.ruleStyle,
          "grammar": props.grammar,
          "gen": props.gen,
          "onChange": (newa, subst: HOTerm.subst) => 
            props.onChange(props.method->Method.setSubproof((),newa), subst), 
        })}
        </div>      
      </div>
    }
}