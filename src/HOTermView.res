open Util
module Make = (
  Atom: AtomDef.ATOM_CHOICE,
  AtomView: AtomDef.ATOM_VIEW with module Atom := Atom,
  HOTerm: HOTerm.S with module Atom := Atom,
) => {
  type idx_props = {idx: int, scope: array<string>, local_scope: array<string>}
  let viewVar = (props: idx_props) =>
    if props.idx < Array.length(props.local_scope) {
      switch props.local_scope[props.idx] {
      | Some(n) if Array.indexOf(props.local_scope, n) == props.idx =>
        <span className="term-boundvar"> {React.string(n)} </span>
      | _ =>
        <span className="term-boundvar-unnamed">
          {React.string("\\")}
          {React.int(props.idx)}
        </span>
      }    
    } else {
      let idx2 = props.idx - Array.length(props.local_scope) 
      switch props.scope[idx2] {
      | Some(n) if Array.indexOf(props.scope, n) == idx2 =>
        <span className="term-metavar"> {React.string(n)} </span>
      | _ =>
        <span className="term-metavar-unnamed">
          {React.string("\\")}
          {React.int(props.idx)}
        </span>
      }    
    }

  let makeMeta = (str: string) =>
    <span className="rule-binder">
      {React.string(str)}
      {React.string(".")}
    </span>
  let makeLocalBinder = (str: string) =>
    <span className="term-binder">
      {React.string(str)}
      {React.string(".")}
    </span>

  let parenthesise = f =>
    [
      <span className="symbol" key={"-1"}> {React.string("(")} </span>,
      ...f,
      <span className="symbol" key={"-2"}> {React.string(")")} </span>,
    ]

  let intersperse = a =>
    a->Array.flatMapWithIndex((e, i) =>
      if i == 0 {
        [e]
      } else {
        [React.string(" "), e]
      }
    )
  type props1 = {term: HOTerm.t, scope: array<string>, local_scope: array<string>, brackets: bool}
  @react.componentWithProps
  let rec make1 = ({term, scope, local_scope,brackets}) =>
    switch term {
    | Var({idx}) => viewVar({idx, scope, local_scope})
    | Symbol({name: s}) => <span className="term-const"> {AtomView.make({atom: s, scope})} </span>
    | Schematic({schematic: s}) =>
      <span className="term-schematic">
        {React.string("?")}
        {React.int(s)}
      </span>
    | App(_) =>
      switch HOTerm.strip(term) {
      | (Symbol({name: s}), args) if HOTerm.isEqualityAtom(s) && Array.length(args) == 2 =>
        <span className="term-equality">
          {React.createElement(make1, {term: args->Array.getUnsafe(0), scope, local_scope, brackets: true})}
          {React.string("=")}
          {React.createElement(make1, {term: args->Array.getUnsafe(1), scope, local_scope, brackets: true})}
        </span>
      | (func, args) =>
        let xs = Array.concat([func], args)
        let a =
          <span className="term-app">
            {xs
            ->Array.mapWithIndex((t, i) =>
              React.createElement(make1, withKey({term: t, scope, local_scope, brackets: true}, i))
            )
            ->intersperse
            ->React.array}
          </span>
        if brackets {
          [a]->parenthesise->React.array
        } else {
          a
        }
      }
    | Lam({name, body}) => {
        let new_scope = Array.concat([name], local_scope)
        <span className="term-lambda">
        {[makeLocalBinder(name),
        React.createElement(make1, {term: body, scope, local_scope: new_scope, brackets: false})]->parenthesise->React.array}
        </span>
      }
    | Unallowed => <p> {React.string("Internal error: unallowed")} </p>
    }
  type props = {term: HOTerm.t, scope: array<string>}
  @react.componentWithProps
  let make = ({term, scope}) => make1({term, scope, local_scope: [], brackets: false})
}
