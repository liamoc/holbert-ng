let symbolSubstitutions: Dict.t<string> = Dict.fromArray([
  ("&&", "∧"),
  ("||", "∨"),
  ("not", "¬"),
  ("->", "→"),
  ("<->", "↔"),
  ("all", "∀"),
  ("exists", "∃"),
  ("top", "⊤"),
  ("bot", "⊥"),
])

let trailingDigitsRE = %re("/^([\s\S]*[^\d])(\^)?(\d+)$/")

let renderTokens = (s: string): array<React.element> => {
  let re = %re("/(§\p{L}+)|(\p{L}+)|(\p{N}+)|([^\p{L}\p{N}§]+)/gu")
  let out = []
  let rec loop = i =>
    switch RegExp.exec(re, s) {
    | None => ()
    | Some(res) =>
      let matches = RegExp.Result.matches(res)
      let bold = matches[0]->Option.getOr("")
      let letters = matches[1]->Option.getOr("")
      let digits = matches[2]->Option.getOr("")
      let other = matches[3]->Option.getOr("")
      let substOrPlain = (token, cls) =>
        switch symbolSubstitutions->Dict.get(token) {
        | Some(sym) => <span key={Int.toString(i)} className="ident-op-lit"> {React.string(sym)} </span>
        | None => <span key={Int.toString(i)} className=cls> {React.string(token)} </span>
        }
      let el =
        if bold != "" {
          <span key={Int.toString(i)} className="ident-bold"> {React.string(String.sliceToEnd(bold, ~start=1))} </span>
        } else if letters != "" {
          substOrPlain(letters, "ident-letters")
        } else if digits != "" {
          <span key={Int.toString(i)} className="ident-digits"> {React.string(digits)} </span>
        } else if other != "" {
          substOrPlain(other, "ident-letters")
        } else {
          React.null
        }
      out->Array.push(el)
      loop(i + 1)
    }
  loop(0)
  out
}

let renderPiece = (piece: string, key: int): React.element =>
  switch RegExp.exec(trailingDigitsRE, piece) {
  | Some(res) =>
    let matches = RegExp.Result.matches(res)
    let base = matches[0]->Option.getOr("")
    let caret = matches[1]->Option.getOr("")
    let digits = matches[2]->Option.getOr("")
    let digitsEl =
      caret != ""
        ? <sup className="ident-sup"> {React.string(digits)} </sup>
        : <sub className="ident-sub"> {React.string(digits)} </sub>
    <span key={Int.toString(key)} className="ident-piece"> {renderTokens(base)->React.array} {digitsEl} </span>
  | None => <span key={Int.toString(key)} className="ident-piece"> {renderTokens(piece)->React.array} </span>
  }

@react.component
let make = (~identifier: string) => {
  let pieces = identifier->String.split("_")
  <span className="ident-root">
    {pieces
    ->Array.mapWithIndex((piece, i) =>
      i == 0
        ? [renderPiece(piece, i)]
        : [
            <span key={`sep${Int.toString(i)}`} className="ident-sep"> {React.string("␣")} </span>,
            renderPiece(piece, i),
          ]
    )
    ->Belt.Array.concatMany
    ->React.array}
  </span>
}