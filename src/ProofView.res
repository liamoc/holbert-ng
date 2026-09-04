open Signatures
open MethodView
@val external computeGoalIndex: Dom.element => int = "computeGoalIndex"
@send external closest: ({..}, string) => Nullable.t<Dom.element> = "closest"
@send external focus: {..} => unit = "focus"
@send external addEventListener: (Dom.element, string, Dom.event => unit) => unit = "addEventListener"
@send external removeEventListener: (Dom.element, string, Dom.event => unit) => unit = "removeEventListener"
module Make = (
  Term: TERM,
  Judgment: JUDGMENT with module Term := Term,
  JudgmentView: JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment,
  MethodView: METHOD_VIEW with module Term := Term and module Judgment := Judgment,
) => {
  
  type ruleStyle = RuleView.style
  let linearStyle = RuleView.Linear
  module RuleView = RuleView.Make(Term, Judgment, JudgmentView)
  
  module Rule = Rule.Make(Term, Judgment)
  module ScopeView = ScopeView.Make(Term, JudgmentView.TermView)
  module Proof = Proof.Make(Term, Judgment, MethodView.Method)
  module Results = Method.MethodResults(Term)
  module ResultsView = {
    type level<'a> = {label: option<Results.label>, nodes: array<Results.t<'a>>}

    type props = {
      ctx: MethodView.Method.Context.t,
      grammar: Term.grammar,
      ruleStyle: ruleStyle,
      initialNodes: array<Results.t<MethodView.Method.t<Proof.checked>>>,
      onApply: (MethodView.Method.t<Proof.checked>, Term.subst) => unit,
    }

    @react.componentWithProps
    let make = (props: props) => {
      let (path, setPath) = React.useState(() => [{label: None, nodes: props.initialNodes}])
      let drillDown = (label, newNodes) =>
        setPath(prev => Array.concat(prev, [{label: Some(label), nodes: newNodes}]))
      let goBack = _ =>
        setPath(prev => Array.length(prev) > 1 ? Array.slice(prev, ~start=0, ~end=Array.length(prev) - 1) : prev)
      let current = path->Belt.Array.getExn(Array.length(path) - 1)

      let rec renderLabel = (label: Results.label, ~suppressRules: bool=false) =>
        switch label {
        | Assumptions => <span className="assumptions-label">{React.string("Assumptions")}</span>
        | Text(s) => React.string(s)
        | Ref(ref) => <RuleRefView ruleRef=ref assms={props.ctx.localFactNames} />
        | RefRule(ref) if suppressRules => <RuleRefView ruleRef=ref assms={props.ctx.localFactNames} />
        | RefRule(ref) =>
          switch props.ctx->MethodView.Method.Context.lookup(ref) {
          | None => React.string("Unknown rule!")
          | Some(rule) =>
            <RuleView rule style=props.ruleStyle grammar=props.grammar scope={props.ctx.fixes}>
              <RuleRefView ruleRef=ref assms={props.ctx.localFactNames} />
            </RuleView>
          }
        | Seq(seq) => seq->Array.map(s => renderLabel(s, ~suppressRules))->React.array
        }

      // A node rendered as an item within the current menu level: bare
      // actions/delays render as buttons; groups render as a labeled
      // section containing their own children, recursively (a group's
      // children can themselves include nested groups/actions/delays).
      let rec renderNode = (node: Results.t<'a>, i: int) =>
        switch node {
        | Action(label, nextTree, subst) =>
          <button tabIndex=0 key={i->Int.toString} onClick={_ => props.onApply(nextTree, subst)}>
            {renderLabel(label)}
          </button>
        | Delay(label, getChildren) =>
          <button tabIndex=0 key={i->Int.toString} onClick={_ => drillDown(label, getChildren())}>
            {renderLabel(label)}
          </button>
        | Group(label, children) =>
          <div className="results-group" key={i->Int.toString}>
            <div className="results-group-label"> {renderLabel(label)} </div>
            <div className="results-group-items">
              {children->Array.mapWithIndex((c, j) => renderNode(c, j))->React.array}
            </div>
          </div>
        }

      <div className="drill-down-container">
        <div className="breadcrumbs">
        {
          let labels = path->Array.filterMap(lvl => lvl.label)
          labels
          ->Array.mapWithIndex((label, i) => {
            let isLast = i == Array.length(labels) - 1
            <div
              key={Int.toString(i)}
              className={`breadcrumb-heading ${isLast ? "breadcrumb-heading-current" : ""}`}
              onClick={isLast ? goBack : ignore}
              tabIndex={isLast ? 0 : (-1)}>
              {renderLabel(label, ~suppressRules=true)}
              {isLast ? <span className="breadcrumb-close typcn typcn-times" /> : React.null}
            </div>
          })
          ->React.array
        }
        </div>
        <div className="menu-options">
          {current.nodes->Array.mapWithIndex((node, i) => renderNode(node, i))->React.array}
          <div className="sidebar-rule"></div>
        </div>
      </div>
    }
  }
  module SidebarView = {
    type props = {
      ctx: MethodView.Method.Context.t,
      goal: Judgment.t,
      grammar: Term.grammar,
      ruleStyle: ruleStyle,
      attached: Results.attached<MethodView.Method.t<Proof.checked>>,
      onApply: (MethodView.Method.t<Proof.checked>, Term.subst) => unit,
      //onBlur: ReactEvent.Focus.t => unit,
    }

    let topLevelNodes = (props: props): array<Results.t<MethodView.Method.t<Proof.checked>>> => {
      let assumptionGroups =
        props.attached.assumptions->Array.map(((i, results)) => {
          //let name = props.ctx.localFactNames[i]->Option.getOr(`#${Int.toString(i)}`)
          Results.Delay(Results.RefRule(Local({index:i})), () => results)
        })
      Array.concat([Results.Group(Results.Assumptions,assumptionGroups)], props.attached.goal)
    }

    @react.componentWithProps
    let make = (props: props) =>
      <div className="sidebar-content resizable-panel">
        <div className="sidebar-goal">
          <ScopeView scope={props.ctx.fixes} editable=None />
          <div className="sidebar-goal-judgment">
          <JudgmentView judgment={props.goal} grammar={props.grammar} scope={props.ctx.fixes} />
          </div>
        </div>
        <ResultsView
          ctx={props.ctx} grammar={props.grammar} ruleStyle={props.ruleStyle}
          initialNodes={topLevelNodes(props)}
          onApply={props.onApply}
        />
      </div>
  }


  
  
  type props = {
    proof: Proof.checked,
    ctx: MethodView.Method.Context.t,
    ruleStyle: ruleStyle,
    grammar: Term.grammar,
    gen: Term.gen,
    onChange: (Proof.checked, Term.subst) => unit,
  }
  
  
  module DisplayModeTabs = {
    type tabProps = {
      display: Proof.display,
      onChange: Proof.display => unit,
    }
    @react.componentWithProps
    let make = (props: tabProps) =>
      <span>
        <div
          className={`display-tab display-tab-right ${props.display == Full ? "display-tab-selected" : ""}`}
          onClick={_ => props.onChange(Full)}>
          <span className="typcn typcn-arrow-maximise" />
        </div>
        <div
          className={`display-tab ${props.display == Tree ? "display-tab-selected" : ""}`}
          onClick={_ => props.onChange(Tree)}>
          <span className="typcn typcn-tree" />
        </div>
        <div
          className={`display-tab display-tab-left ${props.display == Summary ? "display-tab-selected" : ""}`}
          onClick={_ => props.onChange(Summary)}>
          <span className="typcn typcn-arrow-minimise" />
        </div>
      </span>
  }
  module FocusNextContext = {
    type t = {requestFocusIndex: int => unit}

    let context = React.createContext({requestFocusIndex: (_: int) => ()})

    module Provider = {
      let make = React.Context.provider(context)
    }
  }
  module GoalButton = {
    type props = {
      ctx: MethodView.Method.Context.t,
      conclusion: Judgment.t,
      display: Proof.display,
      grammar: Term.grammar,
      gen: Term.gen,
      ruleStyle: ruleStyle,
      onApply: (MethodView.Method.t<Proof.checked>, Term.subst) => unit,
    }

    @react.componentWithProps
    let make = (props: props) => {
      let {sidebarRef} = React.useContext(SidebarContext.context)
      let {requestFocusIndex} = React.useContext(FocusNextContext.context)
      let (isFocused, setFocused) = React.useState(() => false)
      let groupId = React.useId()
      let elRef = React.useRef(Nullable.null)
      let onBlur = e => {
        switch ReactEvent.Focus.relatedTarget(e) {
        | None => () // focus moved to nothing focusable, let outsideclick decide
        | Some(el) =>
          let stillInGroup = el->closest(`.outside-click-group-${groupId}`)->Nullable.toOption->Option.isSome
          if !stillInGroup {
            setFocused(_ => false)
          }
        }
      }
      let setRef = React.useCallback0((node: Nullable.t<Dom.element>): option<unit => unit> => {
        Console.log2("setRef called", node)
        elRef.current = node
        switch node->Nullable.toOption {
        | None => None
        | Some(el) => {
            let handler = _ => {
              setFocused(_ => false)
            }
            el->addEventListener("outsideclick", handler)
            Some(() => el->removeEventListener("outsideclick", handler))
          }
        }
        })
      let portal = switch sidebarRef.current->Nullable.toOption {
      | None => React.null
      | Some(node) =>
        let res = MethodView.Method.apply(props.ctx, props.conclusion, props.gen, rl =>
          Proof.check(
            props.ctx,
            {
              fixes: Term.freshenMetas(~existing=props.ctx.fixes, ~incoming=rl.vars),
              method: None,
              assumptions: Array.fromInitializer(~length=rl.premises->Array.length, i =>
                Int.toString(i + props.ctx.localFacts->Array.length)
              ),
              display: props.display,
            },
            rl,
          )
        )
        
        Portal.createPortal(
          <div className={`outside-click-group-${groupId}`} onBlur>
          <SidebarView ctx={props.ctx} goal={props.conclusion} grammar={props.grammar} ruleStyle={props.ruleStyle} attached=res 
          onApply={(opt, subst) => {
            switch elRef.current->Nullable.toOption {
            | Some(el) => requestFocusIndex(computeGoalIndex(el))
            | None => ()
            }
            props.onApply(opt, subst)
          }}
          /></div>, node)
      }

      <div ref={ReactDOM.Ref.callbackDomRef(setRef)}
        className={`proof-goal outside-click-group-${groupId}`}
        tabIndex=0 onBlur
        onFocus={e => {
          setFocused(_ => true)
          ReactEvent.Focus.stopPropagation(e)
        }}>
        {if isFocused {
          <>
            <span className="button-icon button-icon-blue typcn typcn-location" />
            {portal}
          </>
        } else {
          <span className="button-icon button-icon-blue typcn typcn-location-outline" />
        }}
      </div>
    }
  }
  
  module SummaryView = {
    @react.componentWithProps
    let rec make = (props: props) =>
      switch props.proof {
      | Proof.Checked({fixes, assumptions, method, rule, display}) =>
        switch Proof.enter(props.ctx, {fixes, assumptions, method: None, display}, rule) {
        | Error(_) => <div className="error"> {React.string("context mismatch")} </div>
        | Ok(ctx) => 
          switch method {
          | Do(m) => 
            let subproofs = MethodView.Method.subproofs(m)
            <>
              {MethodView.summary({method: m, ctx,
                  ruleStyle: props.ruleStyle,
                  grammar: props.grammar,
                  gen: props.gen,
                  onChange: (newm, subst) =>
                    props.onChange(Proof.Checked({fixes, assumptions, method: Do(newm), rule, display}), subst),
              })}
              {
              if subproofs->Array.length == 0 {
                React.null
              } else { 
                subproofs->Array.mapWithIndex(((key,child), i) =>                  
                  <span className="proof-summary-node" key={String.make(i)}> 
                    {React.string(i == 0 ? "(" : ",")}
                    {make({...props, ctx, proof: child,
                        onChange: (newChild, subst) =>
                          props.onChange(
                            Proof.Checked({fixes,assumptions,rule,display,
                              method: Do(MethodView.Method.setSubproof(m, key, newChild)),
                            }), subst)
                    })}
                    {i == subproofs->Array.length - 1 ? React.string(")") : React.null }
                  </span>
                  )->React.array
              }
              }
            </>
          | Goal =>
            <GoalButton ctx conclusion=rule.conclusion display gen=props.gen grammar=props.grammar ruleStyle=props.ruleStyle
              onApply={(opt, subst) =>
                props.onChange(Proof.Checked({fixes, assumptions, method: Do(opt), rule, display}), subst)}
            />
          }
        }
      | Proof.ProofError({msg}) => <div className="error"> {React.string(msg)} </div>
      }
  }
  
  module TreeView = {
    @react.componentWithProps
    let rec make = (props: props) =>
      switch props.proof {
      | Proof.Checked({fixes, assumptions, method, rule, display}) =>
        switch Proof.enter(props.ctx, {fixes, assumptions, method: None, display}, rule) {
        | Error(_) => <div className="error"> {React.string("context mismatch")} </div>
        | Ok(ctx) => let count = ref(0)
          <table className="inference">
          <tbody>
          <tr>
            <td className="rule-cell rule-binderbox" rowSpan=3>
            <ScopeView
              scope=fixes
              editable={Some(
                fixes' =>
                  props.onChange(
                    Proof.Checked({fixes: fixes', assumptions, method, rule, display}),
                    Term.makeSubst(),
                  ),
              )}/>
            </td>
            {
              switch method {
              | Do(m) => 
                let subproofs = MethodView.Method.subproofs(m)
                count := subproofs->Array.length
                <>{
                  if subproofs->Array.length == 0 {
                    <td className="rule-cell rule-spacer" />
                  } else { 
                    subproofs->Array.mapWithIndex(((key,child), i) =>
                      <td className="rule-cell rule-premise" key={String.make(i)}>
                      {make({...props, ctx, proof: child,
                        onChange: (newChild, subst) =>
                          props.onChange(
                            Proof.Checked({fixes,assumptions,rule,display,
                              method: Do(MethodView.Method.setSubproof(m, key, newChild)),
                            }), subst)
                      })}
                      </td>                      
                      )->React.array
                  }
                }
                <td rowSpan=3 className="rule-cell rule-rulebox">
                <span className="rule-rulename">{MethodView.summary({method: m, ctx,
                  ruleStyle: props.ruleStyle,
                  grammar: props.grammar,
                  gen: props.gen,
                  onChange: (newm, subst) =>
                    props.onChange(Proof.Checked({fixes, assumptions, method: Do(newm), rule, display}), subst),
                })}</span>
                <span
                  className="button-icon button-icon-red typcn typcn-trash"
                  onClick={_ => props.onChange(Proof.toGoal(props.proof), Term.makeSubst())}
                />
                </td></>
              | Goal => count := 1
                <><td className="rule-cell rule-premise">
                  <GoalButton ctx conclusion=rule.conclusion display 
                    gen=props.gen grammar=props.grammar  ruleStyle=props.ruleStyle
                    onApply={(opt, subst) =>
                      props.onChange(Proof.Checked({fixes, assumptions, method: Do(opt), rule, display}), subst)}
                  />
                </td><td rowSpan=3 className="rule-cell rule-rulebox">{React.string("?")}</td></>
              }
            }
          </tr>
          <tr>
            <td colSpan={count.contents} className="rule-cell rule-conclusion">
            { let i = ref(0)
              let arr = Belt.Array.zipBy(assumptions, rule.premises, (n, r) => {
                i := i.contents + 1
                let thisI = i.contents
                let handleChange = s =>
                  switch Rule.parseRuleName(String.trim(s)) {
                  | Ok((_, "")) => {
                      props.onChange(
                        Proof.Checked({fixes,
                          assumptions: Util.updateAtIndex(assumptions, thisI - 1, s),
                          method, rule, display}),
                        Term.makeSubst(),
                      )
                      Ok(())
                    }
                  | Ok((_, rest)) => Error("Trailing characters "->String.concat(rest))
                  | Error(e) => Error(e)
                  }
                <span className="proof-tree-assumption" key={Int.toString(i.contents - 1)}>
                  {
                    if i.contents > 1 {
                      <span className="term-symbol symbol-comma">{React.string(",")}</span>
                    } else {
                      React.null
                    }
                  }
                  <RuleView rule=r style={linearStyle} scope={ctx.fixes} grammar={props.grammar}>
                    <span className="rule-rulename-local">
                    <EditableLabel label=n onConfirm={handleChange} /></span>
                  </RuleView>
                </span>
              })->React.array
              if i.contents > 0 {
                <>{arr}<span className="term-symbol symbol-turnstile">{React.string("⊢")}</span></>
              } else {
                React.null
              }
            }
            <JudgmentView grammar={props.grammar} judgment={rule.conclusion} scope={ctx.fixes} />
            </td>
          </tr>
          </tbody>
          </table>
        }
      | Proof.ProofError({msg}) => <div className="error"> {React.string(msg)} </div>
      }
  }

  module FullView = {

    let make = (props: props,  ~renderSub: props => React.element) => {
      let {sidebarRef} = React.useContext(SidebarContext.context)
      let (isFocused, setFocused) = React.useState(() => false)


      switch props.proof {
      | Proof.Checked({fixes, assumptions, method, rule, display}) =>
        switch Proof.enter(props.ctx, {fixes, assumptions, method: None, display}, rule) {
        | Error(_) => <div className="error"> {React.string("context mismatch")} </div>
        | Ok(ctx) =>
          <div>
            {if fixes->Array.length != 0 {
              <>
                <span className="proof-text"> {React.string("For any ")} </span>
                <ScopeView
                  scope=fixes
                  editable={Some(
                    fixes' =>
                      props.onChange(
                        Proof.Checked({fixes: fixes', assumptions, method, rule, display}),
                        Term.makeSubst(),
                      ),
                  )}
                />
              </>
            } else {
              <> </>
            }}
            {if assumptions->Array.length != 0 {
              <>
                <span className="proof-text">
                  {React.string(fixes->Array.length != 0 ? "where:" : "Assuming:")}
                </span>
                <ul className={"proof-assumptions proof-assumptions-"->String.concat(String.make(props.ruleStyle))}>
                  { let i = ref(0)
                    Belt.Array.zipBy(assumptions, rule.premises, (n, r) => {
                      i := i.contents + 1
                      let thisI = i.contents
                      let handleChange = s =>
                        switch Rule.parseRuleName(String.trim(s)) {
                        | Ok((_, "")) => {
                            props.onChange(
                              Proof.Checked({fixes,
                                assumptions: Util.updateAtIndex(assumptions, thisI - 1, s),
                                method, rule, display}),
                              Term.makeSubst(),
                            )
                            Ok(())
                          }
                        | Ok((_, rest)) => Error("Trailing characters "->String.concat(rest))
                        | Error(e) => Error(e)
                        }
                      <li key={Int.toString(i.contents - 1)}>
                      
                        <RuleView rule=r style=props.ruleStyle scope={ctx.fixes} grammar={props.grammar}>
                          <span className="rule-rulename-local"><EditableLabel label=n onConfirm={handleChange} /></span>
                        </RuleView>
                      </li>
                    })->React.array}
                </ul>
              </>
            } else {
              <> </>
            }}
            <div className="proof-show">
              <span className="proof-text"> {React.string("Show: ")} </span>
              <span className="proof-judgement">
                <JudgmentView grammar={props.grammar} judgment={rule.conclusion} scope={ctx.fixes} />
              </span>
              {switch method {
              | Goal =>
                <GoalButton
                  ctx
                  conclusion=rule.conclusion 
                  display
                  gen=props.gen grammar=props.grammar  ruleStyle=props.ruleStyle
                  onApply={(opt, subst) =>
                    props.onChange(Proof.Checked({fixes, assumptions, method: Do(opt), rule, display}), subst)}
                />
              | Do(method) =>
                <>
                  <span
                    className="button-icon button-icon-red typcn typcn-trash floating-delete"
                    onClick={_ => props.onChange(Proof.toGoal(props.proof), Term.makeSubst())}
                  />
                  {React.createElement(
                    MethodView.make(p =>
                      renderSub({
                        proof: p["proof"],
                        ctx: p["ctx"],
                        ruleStyle: p["ruleStyle"],
                        grammar: p["grammar"],
                        gen: p["gen"],
                        onChange: p["onChange"],
                      })
                    ),
                    {
                      method,
                      ctx,
                      ruleStyle: props.ruleStyle,
                      grammar: props.grammar,
                      gen: props.gen,
                      onChange: (newm, subst) =>
                        props.onChange(Proof.Checked({fixes, assumptions, method: Do(newm), rule, display}), subst),
                    },
                  )}
                </>
              }}
            </div>
          </div>
        }
      | Proof.ProofError({raw: _, rule: _, msg}) => <div className="error"> {React.string(msg)} </div>
      }
    }
  }  
  
  @react.componentWithProps
  let rec make = (props: props) =>
    switch props.proof {
    | Proof.Checked({fixes, assumptions, method, rule, display}) => {
        let changeDisplay = d =>
          props.onChange(Proof.Checked({fixes, assumptions, method, rule, display: d}), Term.makeSubst())
        <div className="proof-step">
          <DisplayModeTabs display onChange=changeDisplay />
          {switch display {
          | Full => FullView.make(props, ~renderSub= make)
          | Tree => <div className="proof-tree">{TreeView.make(props)}</div>
          | Summary => <div className="proof-summary">{SummaryView.make(props)}
              <span className="button-icon button-icon-red typcn typcn-trash"
                onClick={_ => props.onChange(Proof.toGoal(props.proof), Term.makeSubst())}
              /></div>
          }}
        </div>
      }
    | Proof.ProofError({msg}) => <div className="error"> {React.string(msg)} </div>
    }
  
}
