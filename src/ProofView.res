open Signatures
open MethodView

@send external closest: ({..}, string) => Nullable.t<Dom.element> = "closest"
@send external focus: {..} => unit = "focus"

module Make = (
  Term: TERM,
  Judgment: JUDGMENT with module Term := Term,
  JudgmentView: JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment,
  MethodView: METHOD_VIEW with module Term := Term and module Judgment := Judgment,
) => {
  module Rule = Rule.Make(Term, Judgment)
  module ScopeView = ScopeView.Make(Term, JudgmentView.TermView)
  module Proof = Proof.Make(Term, Judgment, MethodView.Method)
  module Results = Method.MethodResults(Term)
  module ResultsView = {
    type menuState<'a> = {
      history: list<array<Results.t<MethodView.Method.t<Proof.checked>>>>, // Stack of previous menus
      current: array<Results.t<MethodView.Method.t<Proof.checked>>>, // What is currently visible
    }
    type props = {
      initialNodes: array<Results.t<MethodView.Method.t<Proof.checked>>>,
      onApply: (MethodView.Method.t<Proof.checked>, Term.subst) => unit,
      onBlur: ReactEvent.Focus.t => unit,
    }
    @react.componentWithProps
    let make = (props: props) => {
      let (state, setState) = React.useState(_ => {
        history: list{},
        current: props.initialNodes,
      })

      let goBack = _ => {
        setState(prev => {
          switch prev.history {
          | list{parent, ...rest} => {current: parent, history: rest}
          | list{} => prev // Already at the root
          }
        })
      }

      let drillDown = (newNodes: array<Results.t<'a>>) => {
        setState(prev => {
          history: list{prev.current, ...prev.history},
          current: newNodes,
        })
      }

      <div className="drill-down-container">
        {state.history != list{}
          ? <button tabIndex=0 onBlur={props.onBlur} onClick={goBack} className="back-button">
              {React.string("← Back")}
            </button>
          : React.null}

        <div className="menu-options">
          {state.current
          ->Array.mapWithIndex((node, i) => {
            switch node {
            | Action(label, nextTree, subst) =>
              <button
                tabIndex=0
                onBlur={props.onBlur}
                key={label ++ i->Int.toString}
                onClick={_ => props.onApply(nextTree, subst)}
              >
                {React.string(label ++ "")}
              </button>

            | Group(label, children) =>
              <button
                tabIndex=0
                onBlur={props.onBlur}
                key={label ++ i->Int.toString}
                onClick={_ => drillDown(children)}
              >
                {React.string(label ++ " →")}
              </button>

            | Delay(label, getChildren) =>
              <button
                tabIndex=0
                onBlur={props.onBlur}
                key={label ++ i->Int.toString}
                onClick={_ => drillDown(getChildren())}
              >
                {React.string(label ++ " ...")}
              </button>
            }
          })
          ->React.array}
        </div>
      </div>
    }
  }

  type props = {
    proof: Proof.checked,
    ctx: MethodView.Method.Context.t,
    ruleStyle: RuleView.style,
    gen: Term.gen,
    onChange: (Proof.checked, Term.subst) => unit,
  }
  module RuleView = RuleView.Make(Term, Judgment, JudgmentView)
  @react.componentWithProps
  let rec make = (props: props) => {
    let {sidebarRef} = React.useContext(SidebarContext.context)
    let (isFocused, setFocused) = React.useState(() => false)

    let onBlur = e => {
      let leavingProof =
        ReactEvent.Focus.relatedTarget(e)
        ->Option.flatMap(el => el->closest(".sidebar")->Nullable.toOption)
        ->Option.isNone
      if leavingProof {
        setFocused(_ => false)
      }
    }
    switch props.proof {
    | Proof.Checked({fixes, assumptions, method, rule}) => {
        switch Proof.enter(props.ctx, {fixes, assumptions, method: None}, rule) {
        | Error(_) => <div className="error"> {React.string("context mismatch")} </div>
        | Ok(ctx) =>
          <div className="proof-step">
            {if fixes->Array.length != 0 {
                <>            
                <span className="proof-text"> {React.string("For any ")} </span>
                <ScopeView scope=fixes editable={Some(fixes' => {
                   props.onChange(Proof.Checked({fixes:fixes',assumptions,method,rule}), Term.makeSubst())
                   })} />
                </>
            } else {
              <> </>
            }}
            {if assumptions->Array.length != 0 {
              <>
                <span className="proof-text">
                  {React.string(
                    if fixes->Array.length != 0 {
                      "where:"
                    } else {
                      "Assuming:"
                    },
                  )}
                </span>
                <ul
                  className={"proof-assumptions proof-assumptions-"->String.concat(
                    String.make(props.ruleStyle),
                  )}
                >
                  { let i = ref(0);
                    Belt.Array.zipBy(assumptions, rule.premises, (n, r) => {
                      i := i.contents + 1;
                      let handleChange = s => {
                        switch Rule.parseRuleName(String.trim(s)) {
                          | Ok((_,"")) => {
                            props.onChange(
                              Proof.Checked({
                                fixes,
                                assumptions:Util.updateAtIndex(assumptions,i.contents-1,s),
                                method,
                                rule
                              }), Term.makeSubst())
                              Ok(())
                            }
                          | Error(e) => Error(e)
                          }
                        }
                      <li key={Int.toString(i.contents-1)}>
                        <RuleView rule=r style=props.ruleStyle scope={ctx.fixes}>
                        <UIWidgets.EditableLabel label=n onConfirm={handleChange} />
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
                <JudgmentView judgment={rule.conclusion} scope={ctx.fixes} />
              </span>
              {switch method {
              | Goal =>
                let portal = switch sidebarRef.current->Nullable.toOption {
                | None => React.null
                | Some(node) =>
                  let res = MethodView.Method.apply(ctx, rule.conclusion, props.gen, rl => {
                    Proof.check(
                      ctx,
                      {
                        fixes: rl.vars,
                        method: None,
                        assumptions: Array.fromInitializer(~length=rl.premises->Array.length, i =>
                          Int.toString(i + ctx.localFacts->Array.length)
                        ),
                      },
                      rl,
                    )
                  })
                  Portal.createPortal(
                    <>
                      {<ResultsView
                        initialNodes=res
                        onBlur
                        onApply={(opt, subst) =>
                          props.onChange(
                            Proof.Checked({fixes, assumptions, method: Do(opt), rule}),
                            subst,
                          )}
                      >
                      </ResultsView>}
                    </>,
                    node,
                  )
                }
                <div
                  className="proof-goal"
                  tabIndex=0
                  onBlur
                  onFocus={e => {
                    setFocused(_ => true)
                    ReactEvent.Focus.stopPropagation(e)
                  }}
                >
                  {if isFocused {
                    <>
                      <span className="button-icon button-icon-blue typcn typcn-location" />
                      {portal}
                    </>
                  } else {
                    <span className="button-icon button-icon-blue typcn typcn-location-outline" />
                  }}
                </div>
              | Do(method) =>
                <>
                <span className="button-icon button-icon-red typcn typcn-trash floating-delete" onClick={ e=> {
                  props.onChange(Proof.toGoal(props.proof), Term.makeSubst()) }
                  } />
                {
                  React.createElement(
                    MethodView.make(p =>
                      make({
                        proof: p["proof"],
                        ctx: p["ctx"],
                        ruleStyle: p["ruleStyle"],
                        gen: p["gen"],
                        onChange: p["onChange"],
                      })
                    ),
                    {
                      method,
                      ctx,
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
                }              
                </>
              }}
            </div>
          </div>
        }
      }
    | Proof.ProofError({raw: _, rule: _, msg}) => <div className="error"> {React.string(msg)} </div>
    }
  }
}
