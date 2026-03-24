type rec piece =
  | String(string)
  | Var({idx: int})
  | Schematic({schematic: int, allowed: array<int>})
type t = array<piece>

module Atom: AtomDef.ATOM with type t = t
module AtomView: AtomDef.ATOM_VIEW with module Atom := Atom
