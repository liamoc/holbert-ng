module HOTermJ = JudgmentFunctor.HOTermJ
module AxiomS = Editable.TextArea(AxiomSet.Make(HOTerm, HOTermJ, HOTermJView))
module DerivationsOrLemmasView = MethodView.CombineMethodView(
  HOTerm,
  HOTermJ,
  MethodView.DerivationView(HOTerm, HOTermJ),
  MethodView.LemmaView(HOTerm, HOTermJ, HOTermJView),
)
module TheoremS = Editable.TextArea(
  Theorem.Make(HOTerm, HOTermJ, HOTermJView, DerivationsOrLemmasView),
)
module ConfS = ConfigBlock.Make(HOTerm, HOTermJ)

module AxiomStr = Editable.TextArea(StringAxiomSet)
module DerivationsOrLemmasStrView = MethodView.CombineMethodView(
  StringTerm,
  StringTermJudgment,
  MethodView.DerivationView(StringTerm, StringTermJudgment),
  MethodView.LemmaView(StringTerm, StringTermJudgment, StringTermJView),
)
module DLEStrView = MethodView.CombineMethodView(
  StringTerm,
  StringTermJudgment,
  DerivationsOrLemmasStrView,
  MethodView.EliminationView(StringTerm, StringTermJudgment),
)
module TheoremStr = Editable.TextArea(
  Theorem.Make(StringTerm, StringTermJudgment, StringTermJView, DerivationsOrLemmasStrView),
)
