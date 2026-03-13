type stringSymbol = StringS(StringTerm.t) | ConstS(SExp.ConstSymbol.t)
module StringSymbol: SExpFunc.SYMBOL with type t = stringSymbol

include module type of SExpFunc.Make(StringSymbol)
