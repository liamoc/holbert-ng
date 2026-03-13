open Signatures

module Make = (Term: TERM): (
  JUDGMENT with module Term := Term and type t = Term.t and type subst = Term.subst
) => {
  include Term
  type substCodom = Term.t
  let prettyPrintSubstCodom = Term.prettyPrint
  let parseSubstCodom = Term.parse
  let placeSubstCodom = Term.place
  let upshiftSubstCodom = Term.upshift
  let substituteSubstCodom = Term.substitute
  let mapTerms = (t: Term.t, f: Term.t => Term.t): Term.t => f(t)
  let ghostJudgment = Term.ghostTerm
}

module SExpJ = Make(SExp)
module HOTermJ = Make(HOTerm)
module StringSExpJ = Make(StringSExp)
