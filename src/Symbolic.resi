module Base: AtomBase.BASE_ATOM with type t = string
module Atom: AtomDef.ATOM with module Base = Base
module AtomView: AtomDef.ATOM_VIEW with module Atom := Atom
