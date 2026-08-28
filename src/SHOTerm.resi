type rec t =
  | Symbol({name: string, constructor: bool})
  | Var({idx: int})
  | Schematic({schematic: int})
  | Lam({name: string, body: t})
  | App({func: t, arg: t})
  
include Signatures.TERM
  with type t := t
  and type meta = string
  and type schematic = int
  and type subst = Belt.Map.Int.t<t>

let mapTerms: (t, t => t) => t
let strip: t => (t, array<t>)