module type SYMBOL_VIEW = {
  module Symbol: SExpFunc.SYMBOL
  type props = {name: Symbol.t, scope: array<string>}
  let make: props => React.element
}

module Make = (
  Symbol: SExpFunc.SYMBOL,
  SymbolView: SYMBOL_VIEW with module Symbol := Symbol,
  SExp: module type of SExpFunc.Make(Symbol),
): {
  include Signatures.TERM_VIEW with module Term := SExp
} => {
  type props = {term: SExp.t, scope: array<string>}
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
    | Compound({subexps: bits}) =>
      <span className="term-compound">
        {bits
        ->Array.mapWithIndex((t, i) => React.createElement(make, withKey({term: t, scope}, i)))
        ->intersperse
        ->parenthesise
        ->React.array}
      </span>
    | Var({idx}) => viewVar({idx, scope})
    | Symbol(name) =>
      <span className="term-const">
        <SymbolView name scope />
      </span>
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
    | Ghost => React.null
    }
}
