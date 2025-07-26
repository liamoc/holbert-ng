type props = {term: HOTerm.t, scope: array<string>}
open Util
type idx_props = {idx: int, scope: array<string>}
let viewVar = (props: idx_props) =>
  switch props.scope[props.idx] {
  | Some(n) if Array.indexOf(props.scope, n) == props.idx =>
    <span className="term-metavar"> {React.string(n)} </span>
  | _ =>
    <span className="term-metavar-unnamed">
      {React.string("\\")}
      {React.int(props.idx)}
    </span>
  }

let makeMeta = (str: string) =>
  <span className="rule-binder">
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

@react.componentWithProps
let rec make = ({term, scope}) =>
  switch term {
  | Var({idx}) => viewVar({idx, scope})
  | Symbol({name: s}) => <span className="term-const"> {React.string(s)} </span>
  | Schematic({schematic: s, allowed: vs}) =>
    <span className="term-schematic">
      {React.string("?")}
      {React.int(s)}
      <span className="term-schematic-telescope">
        {vs
        ->Array.mapWithIndex((v, i) => React.createElement(viewVar, withKey({idx: v, scope}, i)))
        ->intersperse
        ->parenthesise
        ->React.array}
      </span>
    </span>
  }
