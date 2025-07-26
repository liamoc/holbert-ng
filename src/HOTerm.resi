type rec t =
  | Symbol({name: string})
  | Var({idx: int})
  | Schematic({schematic: int, allowed: array<int>})
  | Lam({name: string, body: t})
  | App({func: t, arg: t})

include Signatures.TERM
  with type t := t
  and type meta = string
  and type schematic = int
  and type subst = Map.t<int, t>
