open Signatures

module Make = (Term: TERM): (JUDGMENT with module Term := Term and type t := Term.t) => {
  include Term
  type substCodom = Term.t
  let prettyPrintSubstVal = Term.prettyPrint
  let parseSubstVal = Term.parse
  let placeSubstVal = Term.place
  let upshiftSubstVal = Term.upshift
  let substituteSubstVal = Term.substitute
}

module SExpJ = {
  type t = SExp.t
  include Make(SExp)
}

module HOTermJ = {
  type t = HOTerm.t
  include Make(HOTerm)
}
