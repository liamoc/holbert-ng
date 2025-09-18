module AxiomS = Editable.TextArea(AxiomSet.Make(SExp, SExp, SExpJView))
module DerivationsOrLemmasView = MethodView.CombineMethodView(
  SExp,
  SExp,
  MethodView.DerivationView(SExp, SExp),
  MethodView.LemmaView(SExp, SExp, SExpJView),
)
module TheoremS = Editable.TextArea(Theorem.Make(SExp, SExp, SExpJView, DerivationsOrLemmasView))
module ConfS = ConfigBlock.Make(SExp, SExp)

module AxiomStr = Editable.TextArea(AxiomSet.Make(StringTerm, StringTermJudgment, StringTermJView))
module DerivationsOrLemmasStrView = MethodView.CombineMethodView(
  StringTerm,
  StringTermJudgment,
  MethodView.DerivationView(StringTerm, StringTermJudgment),
  MethodView.LemmaView(StringTerm, StringTermJudgment, StringTermJView),
)
module TheoremStr = Editable.TextArea(
  Theorem.Make(StringTerm, StringTermJudgment, StringTermJView, DerivationsOrLemmasStrView),
)
