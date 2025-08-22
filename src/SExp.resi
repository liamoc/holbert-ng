type rec t =
  | Symbol({name: string})
  | Compound({subexps: array<t>})
  | Var({idx: int})
  | Schematic({schematic: int, allowed: array<int>})

include Signatures.TERM
  with type t := t
  and type meta = string
  and type schematic = int
  and type subst = Map.t<int, t>

// TODO: confirm that this is the best way to make SExp compatible with new judgement signature
type substVal = t
let upshiftSubstVal: (substVal, int, ~from: int=?) => substVal
let prettyPrintSubstVal: (substVal, ~scope: array<meta>) => string
let parseSubstVal: (string, ~scope: array<meta>, ~gen: gen=?) => result<(substVal, string), string>
