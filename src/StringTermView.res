type props = {term: StringTerm.t, scope: array<string>}
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

let intersperse = a => Util.intersperse(a, ~with=React.string(" "))

module Piece = {
  @react.component
  let make = (~piece: StringTerm.piece, ~scope) =>
    switch piece {
    | Var({idx}) => viewVar({idx, scope})
    | String(s) => <span className="term-const"> {React.string(s)} </span>
    | Schematic({schematic: s, allowed: vs}) =>
      <span className="term-schematic">
        {React.string("?")}
        {React.int(s)}
        <span className="term-schematic-telescope">
          {vs
          ->Array.mapWithIndex((v, i) =>
            React.createElement(viewVar, Util.withKey({idx: v, scope}, i))
          )
          ->intersperse
          ->parenthesise
          ->React.array}
        </span>
      </span>
    }
}

@react.componentWithProps
let make = ({term, scope}) =>
  <span className="term-compound">
    {React.string("\"")}
    {term
    ->Array.mapWithIndex((piece, i) => {
      let key = Int.toString(i)
      <Piece piece scope key />
    })
    ->intersperse
    ->React.array}
    {React.string("\"")}
  </span>
