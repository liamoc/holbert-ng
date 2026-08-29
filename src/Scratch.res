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


module HOTermJView = TermViewAsJudgmentView.Make(HOTerm, HOTerm, HOTermView)
module AxiomS = Editable.TextArea(AxiomSet.Make(HOTerm, HOTerm, HOTermJView))
module InductiveS = Editable.TextArea(HOInductiveSet)
module HORewriteView = RewriteView.Make(HOTerm, HOTerm)
module DerivationsOrLemmasView = MethodView.CombineMethodView(HOTerm,HOTerm,
  MethodView.CombineMethodView(HOTerm, HOTerm,
    MethodView.CombineMethodView(HOTerm, HOTerm,
      HORewriteView, 
      MethodView.CombineMethodView(HOTerm, HOTerm,
        HOMethodsView.ConstructorDisjointnessView,
        HOMethodsView.ConstructorInjectivityView,
      )
    ),
    MethodView.CombineMethodView(HOTerm,HOTerm,
      MethodView.DerivationView(HOTerm, HOTerm),
      MethodView.LemmaView(HOTerm, HOTerm, HOTermJView),
    )
  ),
  MethodView.EliminationView(HOTerm, HOTerm),
)

// Temporarily use DLRView (without Elimination) due to HOTerm unification bug
module TheoremS = Editable.TextArea(Theorem.Make(HOTerm, HOTerm, HOTermJView, DerivationsOrLemmasView))
module ConfS = ConfigBlock.Make(HOTerm, HOTerm)

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
