
open Component

module Term = HOTerm
module Judgment = HOTerm
module Ports = Ports(Term, Judgment)

type state = {grammar: MixfixGrammar.grammar, compiled: MixfixGrammar.compiled }
type props = {
  content: state,
  imports: Ports.t,
  onChange: (state, ~exports: Ports.t=?) => unit,
  reset: unit => unit,
}

let serialise = s => {
  MixfixGrammar.prettyPrintGrammar(s.grammar)
}
let deserialise = (input: string, ~imports as _: Ports.t) => {
  switch MixfixGrammar.parseDecls(String.trim(input)) {
  | Ok((grammar,"")) => 
    switch MixfixGrammar.compile(grammar) {
    | Ok (compiled) => Ok(({grammar,compiled},{Ports.facts: Dict.make(), ruleStyle: None, grammar: compiled}))
    | Error(s) => Error(s)
    }
  | Error(s) => Error(s)
  | Ok((_,s)) => Error("Unable to read from: " + s)
  }
}

let make = props => {
  <pre>{React.string(MixfixGrammar.prettyPrintGrammar(props.content.grammar))}</pre>
}