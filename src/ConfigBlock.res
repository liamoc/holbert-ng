open Signatures
open Component

module Make = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module Ports = Ports(Term, Judgment)
  open RuleView
  type props = {
    content: style,
    imports: Ports.t,
    onChange: (style, ~exports: Ports.t=?) => unit,
  }
  let deserialise = str =>
    switch str {
    | "Gentzen" => Ok((Gentzen, {Ports.facts: Dict.make(), ruleStyle: Some(Gentzen)}))
    | "Linear" => Ok((Linear, {Ports.facts: Dict.make(), ruleStyle: Some(Linear)}))
    | "Hybrid" => Ok((Hybrid, {Ports.facts: Dict.make(), ruleStyle: Some(Hybrid)}))
    | _ => Error("unknown rule style")
    }
  let serialise = style =>
    switch style {
    | Gentzen => "Gentzen"
    | Linear => "Linear"
    | Hybrid => "Hybrid"
    }

  let make = props => {
    let (style, setStyle) = React.useState(_ => props.content)
    let onChange = e => {
      let target = JsxEvent.Form.target(e)
      let value: string = target["value"]
      Console.log("FOO")
      Console.log(value)
      switch deserialise(value) {
      | Ok((sty, _)) => {
          setStyle(_ => sty)
          props.onChange(sty, ~exports={Ports.facts: Dict.make(), ruleStyle: Some(sty)})
        }
      | Error(_) => ()
      }
    }
    <>
      {[Gentzen, Linear, Hybrid]
      ->Array.map(n =>
        <input
          type_="radio"
          id={"style_"->String.concat(serialise(n))}
          key={serialise(n)}
          name="style"
          onChange
          value={serialise(n)}
          checked={style == n}
        />
      )
      ->React.array}
      <p> {React.string(serialise(style))} </p>
    </>
  }
}
