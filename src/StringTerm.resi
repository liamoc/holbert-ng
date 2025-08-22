type rec piece =
  | String(string)
  | Var({idx: int})
  | Schematic({schematic: int, allowed: array<int>})
type t = array<piece>
type subst = Map.t<int, t>

include Signatures.TERM
  with type t := t
  and type meta = string
  and type schematic = int
  and type subst := subst

let fromSExp: SExp.t => t
let toSExp: t => SExp.t
