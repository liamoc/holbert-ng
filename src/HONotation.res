open Component
module Term = HOTerm
module Judgment = HOTerm
module Ports = Ports(Term, Judgment)

type state = {grammar: MixfixGrammar.grammar, compiled: MixfixGrammar.compiled}
type props = {
  content: state,
  imports: Ports.t,
  onChange: (state, ~exports: Ports.t=?) => unit,
  reset: unit => unit,
}

let serialise = (s, ~imports as _: Ports.t) => MixfixGrammar.prettyPrintGrammar(s.grammar)

let deserialise = (input: string, ~imports as _: Ports.t) =>
  switch MixfixGrammar.parseDecls(String.trim(input)) {
  | Ok((grammar, "")) =>
    switch MixfixGrammar.compile(grammar) {
    | Ok(compiled) => Ok(({grammar, compiled}, {Ports.facts: Dict.make(), ruleStyle: None, grammar: compiled}))
    | Error(s) => Error(s)
    }
  | Error(s) => Error(s)
  | Ok((_, s)) => Error("Unable to read from: " ++ s)
  }

module AssocBadge = {
  @react.component
  let make = (~assoc: MixfixGrammar.assoc) => {
    let (label, cls) = switch assoc {
    | Left => ("infixl", "assoc-left")
    | Right => ("infixr", "assoc-right")
    | NonAssoc => ("infix", "assoc-none")
    }
    <span className={`assoc-badge ${cls}`}> {React.string(label)} </span>
  }
}

module OpRow = {
  @react.component
  let make = (~op: MixfixGrammar.opDecl) =>
    <div className="grammar-op-row">
      <AssocBadge assoc={op.assoc} />
      <span className="grammar-op-category"> {React.string(op.category)} </span>
      <span className="grammar-op-name term-symbol"> <IdentifierView identifier={op.name} /> </span>
    </div>
}

module TighterRow = {
  @react.component
  let make = (~tighter: string, ~looser: string) =>
    <div className="grammar-tighter-row">
      <span className="grammar-cat-badge"> {React.string(tighter)} </span>
      <span className="grammar-tighter-arrow"> {React.string("binds tighter than")} </span>
      <span className="grammar-cat-badge"> {React.string(looser)} </span>
    </div>
}

let make = props => {
  let {grammar} = props.content
  <div className="grammar-view">
    {if Array.length(grammar.ops) == 0 {
      React.null
    } else {
      <div className="grammar-section">
        <div className="grammar-section-title"> {React.string("Operators")} </div>
        {grammar.ops->Array.mapWithIndex((op, i) => <OpRow key={Int.toString(i)} op />)->React.array}
      </div>
    }}
    {if Array.length(grammar.tighterThan) == 0 {
      React.null
    } else {
      <div className="grammar-section">
        <div className="grammar-section-title"> {React.string("Precedence")} </div>
        {grammar.tighterThan
        ->Array.mapWithIndex(((tighter, looser), i) =>
          <TighterRow key={Int.toString(i)} tighter looser />
        )
        ->React.array}
      </div>
    }}
    {if Array.length(grammar.ops) == 0 && Array.length(grammar.tighterThan) == 0 {
      <div className="grammar-empty"> {React.string("No declarations")} </div>
    } else {
      React.null
    }}
  </div>
}