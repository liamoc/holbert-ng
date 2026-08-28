module Symbol = AtomDef.MakeAtomChoiceAndView(
  Symbolic.Atom,
  Symbolic.AtomView,
  AtomDef.EmptyAtomChoice,
  AtomDef.EmptyAtomChoiceView,
)
module StringSymbol = AtomDef.MakeAtomChoiceAndView(
  StringA.Atom,
  StringA.AtomView,
  Symbol.Atom,
  Symbol.AtomView,
)
module StringNatSymbol = AtomDef.MakeAtomChoiceAndView(
  AssocComm.Nat.Atom,
  AssocComm.Nat.AtomView,
  StringSymbol.Atom,
  StringSymbol.AtomView,
)
module Final = AtomDef.MakeAtomChoiceAndView(
  Preludic.Atom,
  Preludic.AtomView,
  StringNatSymbol.Atom,
  StringNatSymbol.AtomView,
)

module HOTerm = HOTerm.Make(StringSymbol.Atom)
module HOTermView = HOTermView.Make(StringSymbol.Atom, StringSymbol.AtomView, HOTerm)

module SHOTermJView = TermViewAsJudgmentView.Make(SHOTerm, SHOTerm, SHOTermView)
module AxiomS = Editable.TextArea(AxiomSet.Make(SHOTerm, SHOTerm, SHOTermJView))
module InductiveS = Editable.TextArea(InductiveSet)
module DerivationsOrLemmasView = MethodView.CombineMethodView(
  SHOTerm,
  SHOTerm,
  MethodView.CombineMethodView(
    SHOTerm,
    SHOTerm,
    MethodView.DerivationView(SHOTerm, SHOTerm),
    MethodView.LemmaView(SHOTerm, SHOTerm, SHOTermJView),
  ),
  MethodView.EliminationView(SHOTerm, SHOTerm),
)

// Temporarily use DLRView (without Elimination) due to HOTerm unification bug
module TheoremS = Editable.TextArea(Theorem.Make(SHOTerm, SHOTerm, SHOTermJView, DerivationsOrLemmasView))
module ConfS = ConfigBlock.Make(SHOTerm, SHOTerm)

module StringSExp = SExp.Make(Final.Atom)
module TermView = SExpView.Make(Final.Atom, Final.AtomView, StringSExp)
module StringSExpJView = TermViewAsJudgmentView.Make(StringSExp, StringSExp, TermView)
module AxiomStr = Editable.TextArea(StringAxiomSet.Make(Final.Atom, StringSExp, StringSExpJView))

module DerivationsOrLemmasStrView = MethodView.CombineMethodView(
  StringSExp,
  StringSExp,
  MethodView.DerivationView(StringSExp, StringSExp),
  MethodView.LemmaView(StringSExp, StringSExp, StringSExpJView),
)
module DLEStrView = MethodView.CombineMethodView(
  StringSExp,
  StringSExp,
  DerivationsOrLemmasStrView,
  MethodView.EliminationView(StringSExp, StringSExp),
)
module TheoremStr = Editable.TextArea(
  Theorem.Make(StringSExp, StringSExp, StringSExpJView, DLEStrView),
)
