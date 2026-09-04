
@val external focusGoalNearIndex: (Dom.element, int) => unit = "focusGoalNearIndex"
open Signatures
open Component
open MethodView

module Make = (
  Term: TERM,
  Judgment: JUDGMENT with module Term := Term,
  JudgmentView: JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment,
  MethodView: METHOD_VIEW with module Term := Term and module Judgment := Judgment,
) => {
  module Rule = Rule.Make(Term, Judgment)
  module Proof = Proof.Make(Term, Judgment, MethodView.Method)
  module Context = Method.Context(Term, Judgment)
  module ProofView = ProofView.Make(Term, Judgment, JudgmentView, MethodView)
  open RuleView
  module RuleView = RuleView.Make(Term, Judgment, JudgmentView)
  module Ports = Ports(Term, Judgment)
  type state = {
    name: string,
    rule: Rule.t,
    proof: Proof.t,
    gen: Term.gen,
    substFailed: option<string>,
  }
  type props = {
    content: state,
    imports: Ports.t,
    onChange: (state, ~exports: Ports.t=?) => unit,
    reset: unit => unit,
  }
  let serialise = (state: state, ~imports: Ports.t) => {
    state.rule
    ->Rule.prettyPrintTopLevel(~name=state.name,~grammar=imports.grammar)
    ->String.concat("\n\n")
    ->String.concat(Proof.prettyPrint(state.proof, ~grammar=imports.grammar, ~scope=[], ~assms=[]))
  }
  let deserialise = (str: string, ~imports: Ports.t) => {
    let _facts = imports.facts
    let gen = Term.makeGen()
    let cur = ref(str)
    switch Rule.parseTopLevel(cur.contents, ~grammar=imports.grammar, ~scope=[], ~gen) {
    | Error(e) => Error(e)
    | Ok(((rule, name), s)) =>
      switch Proof.parse(s, ~grammar=imports.grammar, ~scope=[], ~assms=[], ~gen) {
      | Error(e) => Error(e)
      | Ok((_, s')) if String.length(String.trim(s')) > 0 =>
        Error("Trailing input: "->String.concat(s'))
      | Ok((proof, _)) =>
        Ok((
          {name, rule, proof, gen, substFailed: None},
          {Ports.facts: Dict.fromArray([(name, rule)]), ruleStyle: None, grammar: Term.emptyGrammar},
        ))
      }
    }
  }
  let make = props => {
    let ruleStyle = props.imports.ruleStyle->Option.getOr(Hybrid)
    let ctx: Context.t = {fixes: [], globalFacts: props.imports.facts, localFacts: [], localFactNames: []}
    let checked = Proof.check(ctx, props.content.proof, props.content.rule)
    let sidebarRef = React.useRef(Nullable.null)

    let rootRef = React.useRef(Nullable.null)
    let pendingFocusIndex = React.useRef(-1)
    
    React.useLayoutEffect1(() => {
      if pendingFocusIndex.current >= 0 {
        let idx = pendingFocusIndex.current
        pendingFocusIndex.current = -1
        switch rootRef.current->Nullable.toOption {
        | Some(el) => focusGoalNearIndex(el, idx)
        | None => ()
        }
      }
      None
    }, [props.content.proof])

    let requestFocusIndex = (index: int) => {
      pendingFocusIndex.current = index
    }
    
    let proofChanged = (proof, subst) => {
      let proof = Proof.uncheck(proof)->Proof.substitute(subst)
      props.onChange(
        try {
          Proof.check(ctx, proof, props.content.rule)->ignore
          {...props.content, proof, substFailed: None}
        } catch {
        | SExp.SubstNotCompatible(s) => {...props.content, substFailed: Some(s)}
        },
        ~exports={
          Ports.facts: Dict.fromArray([(props.content.name, props.content.rule)]),
          ruleStyle: None,
          grammar: Term.emptyGrammar,
        },
      )
    }    
    <SidebarContext sidebarRef>
      <RuleView rule={props.content.rule} grammar={props.imports.grammar} scope={[]} style={ruleStyle}>
          <span className="rule-rulename-global"><IdentifierView identifier=props.content.name /></span>
      </RuleView>
      <h4> {React.string("Proof")} </h4>
      <ProofView.FocusNextContext.Provider value={{requestFocusIndex:requestFocusIndex}}>
      <div className="theorem-instance" ref={ReactDOM.Ref.domRef(rootRef)} ><ProofView
        ruleStyle={ruleStyle} ctx={ctx} grammar={props.imports.grammar} proof=checked gen={props.content.gen} onChange=proofChanged
      />
      {switch props.content.substFailed {
      | Some(msg) => React.string(msg)
      | None => React.null
      }}</div>
      <div className="sidebar watch-outside-click" ref={ReactDOM.Ref.domRef(sidebarRef)} />
      </ProofView.FocusNextContext.Provider>
    </SidebarContext>
  }
}
