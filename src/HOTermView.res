
let makeMeta = (str: string) =>
  <span className="rule-binder">
    {React.string(str)}
    {React.string(".")}
  </span>
let makeEditableMeta = (str: string,~onChange: string => unit ) => {
  let handleConfirm = s => switch HOTerm.parseMeta(String.trim(s)->String.concat(".")) {
    | Ok((s',"")) => {onChange(s'); Ok(())}
    | Error(e) => Error(e)
    | Ok((_,rest)) => Error("trailing string after meta name: "->String.concat(rest))
    }
  <span className="rule-binder">
    <UIWidgets.EditableLabel label={str} onConfirm={handleConfirm} />
    {React.string(".")}
  </span>  
}

type props = {term: HOTerm.t, grammar: HOTerm.grammar, scope: array<string>}
module JsxTarget: MixfixPrinter.PRINT_TARGET with type out = React.element = {
  type out = React.element
  let symbolFont = s =>  <span className="term-op-lit">{React.string(s)}</span>
  let renderSymbol = s => switch s {
  | "&&" => symbolFont("∧")
  | "||" => symbolFont("∨")
  | "not" => symbolFont("¬")
  | "->" => symbolFont("→")
  | "<->" => symbolFont("↔")
  | "all" => symbolFont("∀")
  | "exists" => symbolFont("∃")
  | "top" => symbolFont("⊤")
  | "bot" => symbolFont("⊥")
  | s => <span className="term-symbol">{React.string(s)}</span>
  }
  
  let leaf = (~kind, s) => {
    if kind == "constructor" {
       <span className={`term-${kind}`}>{React.string(s->String.sliceToEnd(~start=1))}</span>
    } else if kind == "symbol" || kind == "op-lit" {
      renderSymbol(s)
    } else {
      <span className={`term-${kind}`}>{React.string(s)}</span>
    }
  } 
  let keyed = (el: React.element, i : int): React.element =>
    <React.Fragment key={Int.toString(i)}> {el} </React.Fragment>
  let seq = arr => arr->Array.mapWithIndex(keyed)->React.array
  let spaced = arr => {
    let interspersed =
      arr
      ->Array.mapWithIndex((el, i) => i == 0 ? [el] : [React.string(" "), el])
      ->Belt.Array.concatMany
    interspersed->Array.mapWithIndex(keyed)->React.array
  }
  let parens = el => <span>
    <span className="term-lambda-punct"> {React.string("(")} </span> {el} <span className="term-lambda-punct"> {React.string(")")} </span>
    </span>
}

module JsxPrinter = MixfixPrinter.Make(JsxTarget, HOTerm.PrintLeaf(JsxTarget))

@react.componentWithProps
let make = ({term, grammar, scope}) => 
  JsxPrinter.prettyPrintWithGrammar(term, ~parentheses=false,~grammar, ~scope)
