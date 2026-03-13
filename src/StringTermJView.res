module StringSymbolView: SExpViewFunc.SYMBOL_VIEW with module Symbol := StringSExp.StringSymbol = {
  type props = {name: StringSExp.StringSymbol.t, scope: array<string>}
  let make = ({name, scope}: props) =>
    switch name {
    | StringSExp.StringS(term) => <StringTermView term scope />
    | StringSExp.ConstS(name) => <SExpView.ConstSymbolView name scope />
    }
}

module View = SExpViewFunc.Make(StringSExp.StringSymbol, StringSymbolView, StringSExp)

module TermView = View
type props = {
  judgment: StringSExp.t,
  scope: array<string>,
}
let make = ({judgment, scope}) => View.make({term: judgment, scope})
