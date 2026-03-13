module HOTermJ = TermAsJudgment.HOTermJ
module StringSExpJ = TermAsJudgment.StringSExpJ

module AxiomS = Editable.TextArea(AxiomSet.Make(HOTerm, HOTermJ, HOTermJView))
module InductiveS = Editable.TextArea(InductiveSet.Make(HOTerm, HOTermJ, HOTermJView))

module EqualityViews = MethodView.CombineMethodView(
  HOTerm,
  HOTermJ,
  MethodView.RewriteView(HOTermJ),
  MethodView.RewriteReverseView(HOTermJ),
)
module ConstructorEqualityViews = MethodView.CombineMethodView(
  HOTerm,
  HOTermJ,
  EqualityViews,
  MethodView.ConstructorNeqView(HOTermJ),
)
module RewritesView = MethodView.CombineMethodView(
  HOTerm,
  HOTermJ,
  ConstructorEqualityViews,
  MethodView.ConstructorInjView(HOTermJ),
)
module DerivationsOrLemmasView = MethodView.CombineMethodView(
  HOTerm,
  HOTermJ,
  MethodView.CombineMethodView(
    HOTerm,
    HOTermJ,
    MethodView.DerivationView(HOTerm, HOTermJ),
    MethodView.LemmaView(HOTerm, HOTermJ, HOTermJView),
  ),
  MethodView.EliminationView(HOTerm, HOTermJ),
)
module DLRView = MethodView.CombineMethodView(
  HOTerm,
  HOTermJ,
  DerivationsOrLemmasView,
  RewritesView,
)
module DLREView = MethodView.CombineMethodView(
  HOTerm,
  HOTermJ,
  DLRView,
  MethodView.EliminationView(HOTerm, HOTermJ),
)

// Temporarily use DLRView (without Elimination) due to HOTerm unification bug
module TheoremS = Editable.TextArea(Theorem.Make(HOTerm, HOTermJ, HOTermJView, DLRView))
module ConfS = ConfigBlock.Make(HOTerm, HOTermJ)

module AxiomStr = Editable.TextArea(StringAxiomSet)
module DerivationsOrLemmasStrView = MethodView.CombineMethodView(
  StringSExp,
  StringSExpJ,
  MethodView.DerivationView(StringSExp, StringSExpJ),
  MethodView.LemmaView(
    StringSExp,
    StringSExpJ,
    StringTermJView,
  ),
)
module DLEStrView = MethodView.CombineMethodView(
  StringSExp,
  StringSExpJ,
  DerivationsOrLemmasStrView,
  MethodView.EliminationView(StringSExp, StringSExpJ),
)
module TheoremStr = Editable.TextArea(
  Theorem.Make(
    StringSExp,
    StringSExpJ,
    StringTermJView,
    DLEStrView,
  ),
)
