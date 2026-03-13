module ConstSymbol = SExp.ConstSymbol
module ConstSymbolView: SExpViewFunc.SYMBOL_VIEW with module Symbol := SExp.ConstSymbol = {
  type props = {name: ConstSymbol.t, scope: array<string>}
  let make = (props: props) => React.string(props.name)
}

include SExpViewFunc.Make(ConstSymbol, ConstSymbolView, SExp)
