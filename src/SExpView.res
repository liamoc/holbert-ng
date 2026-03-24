module type ATOM_VIEW = AtomDef.ATOM_VIEW

module Make = (
  Atom: AtomDef.COERCIBLE_ATOM,
  AtomView: ATOM_VIEW with module Atom := Atom,
  SExp: module type of SExp.Make(Atom),
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
    | Atom(name) =>
      <span className="term-const">
        <AtomView name scope />
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
    }
}
