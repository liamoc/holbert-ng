module AxiomS = Editable.TextArea(AxiomSet.Make(HOTerm, HOTerm, HOTermJView))
module InductiveS = Editable.TextArea(InductiveSet.Make(HOTerm, HOTerm, HOTermJView))
module DerivationsOrLemmasView = MethodView.CombineMethodView(
  HOTerm,
  HOTerm,
  MethodView.DerivationView(HOTerm, HOTerm),
  MethodView.LemmaView(HOTerm, HOTerm, HOTermJView),
)
module TheoremS = Editable.TextArea(
  Theorem.Make(HOTerm, HOTerm, HOTermJView, DerivationsOrLemmasView),
)
module ConfS = ConfigBlock.Make(HOTerm, HOTerm)
