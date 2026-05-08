module Make: (
  Atom: AtomDef.ATOM_CHOICE,
  AtomView: AtomDef.ATOM_VIEW with module Atom := Atom,
  HOTerm: HOTerm.S with module Atom := Atom,
) => (Signatures.TERM_VIEW with module Term := HOTerm)
