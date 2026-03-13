module ConstSymbolView: SExpViewFunc.SYMBOL_VIEW with module Symbol := SExp.ConstSymbol

include Signatures.TERM_VIEW with module Term := SExp
