open Signatures
open Method
module type METHOD_VIEW = {
  module Term: TERM
  module Judgment: JUDGMENT with module Term := Term
  module Method: PROOF_METHOD with module Term := Term and module Judgment := Judgment
  type props<'a> = {
    method: Method.t<'a>,
    ctx:Method.Context.t,
    ruleStyle: RuleView.style,
    grammar: Term.grammar,
    gen: Term.gen,
    onChange: (Method.t<'a>, Term.subst) => unit,
  }
  type srProps<'a> = {
    "proof": 'a,
    "ctx": Method.Context.t,
    "ruleStyle": RuleView.style,
    "grammar": Term.grammar,
    "gen": Term.gen,    
    "onChange": ('a, Term.subst) => unit,
  }
  let make: (srProps<'a> => React.element) => props<'a> => React.element
  let summary: props<'a> => React.element
}

module RuleRefView = {

  @react.component
  let make = (~assms: array<string>, ~ruleRef: RuleRef.t) => {
    switch ruleRef {
    | Local({index}) =>
      switch assms[index] {
      | None =>
        <span className="rule-ref rule-ref--invalid"> {React.string(`#${Belt.Int.toString(index)}`)} </span>
      | Some(name) =>
        let lastIndexWithName = assms->Belt.Array.reduceWithIndex(-1, (acc, n, i) =>
          n == name ? i : acc
        )
        let shadowed = lastIndexWithName != index
        if shadowed {
          <span className="rule-ref rule-ref--local rule-ref--shadowed">
            {React.string(name)}
            <span className="rule-ref__index"> {React.string(`@${Belt.Int.toString(index)}`)} </span>
          </span>
        } else {
          <span className="rule-ref rule-ref--local"> {React.string(name)} </span>
        }
      }
    | Global({name}) =>
      let shadowedByLocal = assms->Belt.Array.some(n => n == name)
      if shadowedByLocal {
        <span className="rule-ref rule-ref--global rule-ref--shadowed">
          <span className="rule-ref__global-marker"> {React.string("global:")} </span>
          {React.string(name)}
        </span>
      } else {
        <span className="rule-ref rule-ref--global"> {React.string(name)} </span>
      }
    }
  }
}



module DerivationView = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module Method = Derivation(Term, Judgment)
  type props<'a> = {
    method: Method.t<'a>,
    ctx: Method.Context.t,
    ruleStyle: RuleView.style,
    grammar: Term.grammar,    
    gen: Term.gen,
    onChange: (Method.t<'a>, Term.subst) => unit,
  }
  type srProps<'a> = {
    "proof": 'a,
    "ctx": Method.Context.t,
    "ruleStyle": RuleView.style,
    "grammar": Term.grammar,    
    "gen": Term.gen,
    "onChange": ('a, Term.subst) => unit,
  }
  let summary = props => <RuleRefView ruleRef=props.method.ruleName assms=props.ctx.localFactNames />
  let make = (subRender: srProps<'a> => React.element) =>
    props => {
      <div>
        <b> {React.string("by ")} </b>
        <RuleRefView ruleRef=props.method.ruleName assms=props.ctx.localFactNames />
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
                  "grammar": props.grammar,
                  "gen": props.gen,
                  "onChange": (newa, subst: Term.subst) =>
                    props.onChange(props.method->Method.setSubproof(i, newa), subst),
                },
              )}
            </li>
          })
          ->React.array}
        </ul>
      </div>
    }
}

module EliminationView = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module Method = Elimination(Term, Judgment)
  type props<'a> = {
    method: Method.t<'a>,
    ctx: Method.Context.t,
    ruleStyle: RuleView.style,
    grammar: Term.grammar,
    gen: Term.gen,
    onChange: (Method.t<'a>, Term.subst) => unit,
  }
  type srProps<'a> = {
    "proof": 'a,
    "ctx": Method.Context.t,
    "ruleStyle": RuleView.style,
    "grammar": Term.grammar,
    "gen": Term.gen,
    "onChange": ('a, Term.subst) => unit,
  }
  let summary = props => <span>{React.string("elim ")}<RuleRefView ruleRef=props.method.ruleName assms=props.ctx.localFactNames /></span>
  
  let make = (subRender: srProps<'a> => React.element) => props => {
      <div>
        <b> {React.string("elim ")} </b>
        <RuleRefView ruleRef=props.method.ruleName assms=props.ctx.localFactNames />
        <span className="spacer">{React.string(" ")}</span>
        <RuleRefView ruleRef=props.method.elimName assms=props.ctx.localFactNames />
        <ul className="subgoals">
          {props.method.subgoals->Array.mapWithIndex((sg, i) => {
            <li key={String.make(i)}>
              {React.createElement(
                subRender,
                {
                  "proof": sg,
                  "ctx": props.ctx,
                  "ruleStyle": props.ruleStyle,
                  "grammar": props.grammar,                  
                  "gen": props.gen,
                  "onChange": (newa, subst: Term.subst) =>
                    props.onChange(props.method->Method.setSubproof(i, newa), subst),
                },
              )}
            </li>
          })
          ->React.array}
        </ul>
      </div>
    }
}

module LemmaView = (
  Term: TERM,
  Judgment: JUDGMENT with module Term := Term,
  JudgmentView: JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment,
) => {
  module Method = Lemma(Term, Judgment)
  type props<'a> = {
    method: Method.t<'a>,
    ctx: Method.Context.t,
    ruleStyle: RuleView.style,
    grammar: Term.grammar,
    gen: Term.gen,
    onChange: (Method.t<'a>, Term.subst) => unit,
  }
  type srProps<'a> = {
    "proof": 'a,
    "ctx": Method.Context.t,
    "ruleStyle": RuleView.style,
    "grammar": Term.grammar,
    "gen": Term.gen,    
    "onChange": ('a, Term.subst) => unit,
  }
  let summary = props => <span>{React.string("lemma")}</span>
  module RuleView = RuleView.Make(Term, Judgment, JudgmentView)
  let make = (subRender: srProps<'a> => React.element) =>
    props => {
      <div>
        <b> {React.string("have ")} </b>
        <RuleView rule={props.method.rule} scope={props.ctx.fixes} grammar={props.grammar} style={props.ruleStyle}>
          {React.null}
        </RuleView>
        {React.createElement(
          subRender,
          {
            "proof": props.method.proof,
            "ctx": props.ctx,
            "ruleStyle": props.ruleStyle,
            "grammar": props.grammar,            
            "gen": props.gen,
            "onChange": (proof, subst) => {props.onChange({...props.method, proof}, subst)},
          },
        )}
        {React.createElement(
          subRender,
          {
            "proof": props.method.show,
            "ctx": props.ctx,
            "ruleStyle": props.ruleStyle,
            "grammar": props.grammar,
            "gen": props.gen,            
            "onChange": (show, subst) => {props.onChange({...props.method, show}, subst)},
          },
        )}
      </div>
    }
}

module CombineMethodView = (
  Term: TERM,
  Judgment: JUDGMENT with module Term := Term,
  Method1View: METHOD_VIEW with module Term := Term and module Judgment := Judgment,
  Method2View: METHOD_VIEW
    with module Term := Term
    and module Judgment := Judgment
    and type srProps<'a> = Method1View.srProps<'a>,
) => {
  module Method = Combine(Term, Judgment, Method1View.Method, Method2View.Method)
  type props<'a> = {
    method: Method.t<'a>,
    ctx: Method.Context.t, 
    ruleStyle: RuleView.style,
    grammar: Term.grammar,
    gen: Term.gen,
    onChange: (Method.t<'a>, Term.subst) => unit,
  }
  type srProps<'a> = Method1View.srProps<'a>
  let summary = props =>
      switch props.method {
      | First(m) => Method1View.summary({
          method: m,
          ctx: props.ctx,
          ruleStyle: props.ruleStyle,
          grammar: props.grammar,
          gen: props.gen,
          onChange: (n, s) => props.onChange(First(n), s),
        })
      | Second(m) => Method2View.summary({
          method: m,
          ctx: props.ctx,
          ruleStyle: props.ruleStyle,
          grammar: props.grammar,
          gen: props.gen,
          onChange: (n, s) => props.onChange(Second(n), s),
        })
      }
  let make = (subrender: srProps<'a> => React.element) =>
    props => {
      switch props.method {
      | First(m) =>
        Method1View.make(subrender)({
          method: m,
          ctx: props.ctx,
          ruleStyle: props.ruleStyle,
          grammar: props.grammar,
          gen: props.gen,
          onChange: (n, s) => props.onChange(First(n), s),
        })
      | Second(m) =>
        Method2View.make(subrender)({
          method: m,
          ctx: props.ctx,
          ruleStyle: props.ruleStyle,
          grammar: props.grammar,          
          gen: props.gen,
          onChange: (n, s) => props.onChange(Second(n), s),
        })
      }
    }
}
