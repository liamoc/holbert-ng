module type ATOM_VIEW = AtomDef.ATOM_VIEW

module Make = (
  Atom: AtomDef.ATOM,
  AtomView: ATOM_VIEW with module Atom := Atom,
  SExp: module type of SExp.Make(Atom),
): {
  include Signatures.TERM_VIEW with module Term := SExp
} => {
  type props = {term: SExp.t, grammar: unit, scope: array<string>}
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
      <IdentifierView identifier=str />
      {React.string(".")}
    </span>
  let makeEditableMeta = (str: string,~onChange: string => unit ) => {
    let handleConfirm = s => switch SExp.parseMeta(String.trim(s)) {
      | Ok((s',"")) => {onChange(s'); Ok(())}
      | Error(e) => Error(e)
      | Ok((_,rest)) => Error(`Trailing text: ${rest}`)
      }
    <span className="rule-binder">
      <EditableLabel label={str} onConfirm={handleConfirm} />
      {React.string(".")}
    </span>  
  }
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

  module rec Inner: {
    @react.component
    let make: (~term: SExp.t, ~scope: array<string>, ~parens: bool=?) => React.component
  } = {
    @react.component
    let make = (~term: SExp.t, ~scope: array<string>, ~parens: bool=true) =>
      switch term {
      | Compound({subexps: bits}) =>
        <span className="term-compound">
          {bits
          ->Array.mapWithIndex((t, i) => <Inner term={t} scope key={i->Int.toString} />)
          ->intersperse
          ->(a =>
            if parens {
              parenthesise(a)
            } else {
              a
            })
          ->React.array}
        </span>
      | Var({idx}) => viewVar({idx, scope})
      | Atom(atom) =>
        <span className="term-const">
          <AtomView atom scope />
        </span>
      | Schematic({schematic: s, allowed: vs}) =>
        <span className="term-schematic">
          {React.string("?")}
          {React.int(s)}
          <span className="term-schematic-telescope">
            {vs
            ->Array.mapWithIndex((v, i) =>
              React.createElement(viewVar, withKey({idx: v, scope}, i))
            )
            ->intersperse
            ->parenthesise
            ->React.array}
          </span>
        </span>
      }
  }
  @react.componentWithProps
  let make = ({term, scope}) => <Inner term scope parens={false} />
}
