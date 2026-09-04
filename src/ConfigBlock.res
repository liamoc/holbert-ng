open Signatures
open Component
module Make = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module Ports = Ports(Term, Judgment)
  open RuleView

  type props = {
    content: style,
    imports: Ports.t,
    onChange: (style, ~exports: Ports.t=?) => unit,
    reset: unit => unit,
  }

  let deserialise = str =>
    switch str {
    | "Gentzen" => Ok((Gentzen, {Ports.facts: Dict.make(), ruleStyle: Some(Gentzen), grammar: Term.emptyGrammar}))
    | "Linear" => Ok((Linear, {Ports.facts: Dict.make(), ruleStyle: Some(Linear), grammar: Term.emptyGrammar}))
    | "Hybrid" => Ok((Hybrid, {Ports.facts: Dict.make(), ruleStyle: Some(Hybrid), grammar: Term.emptyGrammar}))
    | _ => Error("unknown rule style")
    }

  let serialise = style =>
    switch style {
    | Gentzen => "Gentzen"
    | Linear => "Linear"
    | Hybrid => "Hybrid"
    }

  let describe = style =>
    switch style {
    | Gentzen => "Full tree layout including hypothetical derivations"
    | Linear => "A single-line rule format"
    | Hybrid => "A tree layout where hypothetical derivations are rendered in Linear style"
    }

  let make = props => {
    let (style, setStyle) = React.useState(_ => props.content)

    let onChange = e => {
      let target = JsxEvent.Form.target(e)
      let value: string = target["value"]
      switch deserialise(value) {
      | Ok((sty, _)) => {
          setStyle(_ => sty)
          props.onChange(sty, ~exports={Ports.facts: Dict.make(), ruleStyle: Some(sty), grammar: Term.emptyGrammar})
        }
      | Error(_) => ()
      }
    }

    <div className="settings-panel">
      <div className="settings-row">
        <label className="settings-label" htmlFor="rule-style-select"> {React.string("Rule display style")} </label>
        <select id="rule-style-select" className="settings-select" value={serialise(style)} onChange>
          {[Gentzen, Linear, Hybrid]
          ->Array.map(n => <option key={serialise(n)} value={serialise(n)}> {React.string(serialise(n))} </option>)
          ->React.array}
        </select>
        <div className="settings-desc"> {React.string(describe(style))} </div>
      </div>
    </div>
  }
}