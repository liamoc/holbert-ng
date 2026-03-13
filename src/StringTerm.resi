type rec piece =
  | String(string)
  | Var({idx: int})
  | Schematic({schematic: int, allowed: array<int>})
  | Ghost
type t = array<piece>
type subst = Map.t<int, t>
type gen = ref<int>

include Signatures.TERM
  with type t := t
  and type meta = string
  and type schematic = int
  and type subst := subst
  and type gen := gen
