type t = (StringTerm.t, SExp.t)

include Signatures.JUDGMENT with module Term := StringTerm and type t := t
