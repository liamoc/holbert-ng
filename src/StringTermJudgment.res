type t = (StringTerm.t, SExp.t)
type substCodom = StringV(StringTerm.t) | SExpV(SExp.t)
type subst = Map.t<int, substCodom>
type schematic = int
type meta = string

let mapSubst = Util.mapMapValues
let mergeSubsts: (subst, subst) => subst = Util.mapUnion
let splitSub: subst => (StringTerm.subst, SExp.subst) = s => {
  let stringSub = Map.make()
  let judgeSub = Map.make()
  s->Map.forEachWithKey((v, k) => {
    switch v {
    | StringV(t) => stringSub->Map.set(k, t)
    | SExpV(t) => judgeSub->Map.set(k, t)
    }
  })
  (stringSub, judgeSub)
}
let substitute = ((term, judge): t, sub) => {
  let (stringSub, judgeSub) = splitSub(sub)
  (StringTerm.substitute(term, stringSub), SExp.substitute(judge, judgeSub))
}

let substituteSubstVal = (s: substCodom, subst: subst) => {
  let (stringSub, judgeSub) = splitSub(subst)
  switch s {
  | StringV(t) => StringV(StringTerm.substitute(t, stringSub))
  | SExpV(t) => SExpV(SExp.substitute(t, judgeSub))
  }
}
let equivalent = ((t1, j1): t, (t2, j2): t) =>
  StringTerm.equivalent(t1, t2) && SExp.equivalent(j1, j2)
let unify = ((t1, j1): t, (t2, j2): t, ~gen as _=?) => {
  // cartesian prod of possible unifications
  let judgeSubs = SExp.unify(j1, j2)->Seq.map(s => s->Util.mapMapValues(t => SExpV(t)))
  let stringSubs = StringTerm.unify(t1, t2)->Seq.map(s => s->Util.mapMapValues(t => StringV(t)))
  judgeSubs->Seq.flatMap(judgeSub =>
    // NOTE: silent failure mode here where substitution exists for a given schematic on both string
    // SExp side. for now, bias string sub. in future, maybe consider this not a valid judgement to begin with.
    stringSubs->Seq.map(stringSub => Util.mapUnion(stringSub, judgeSub))
  )
}
let substDeBruijn = ((t, j): t, scope: array<substCodom>, ~from: int=0) => {
  // NOTE: implicit type coercion here. if we unify and expect a string but get an sexp,
  // perform naive flattening of compound to substitute. likewise in opposite direction.
  let stringScope = scope->Array.map(v =>
    switch v {
    | StringV(t) => t
    | SExpV(t) => StringTerm.fromSExp(t)
    }
  )
  let judgeScope = scope->Array.map(v =>
    switch v {
    | SExpV(t) => t
    | StringV(t) => StringTerm.toSExp(t)
    }
  )
  (StringTerm.substDeBruijn(t, stringScope, ~from), SExp.substDeBruijn(j, judgeScope, ~from))
}
let upshift = ((t, j): t, amount: int, ~from: int=0) => (
  StringTerm.upshift(t, amount, ~from),
  SExp.upshift(j, amount, ~from),
)

let upshiftSubstVal = (v: substCodom, amount: int, ~from: int=0) =>
  switch v {
  | StringV(t) => StringV(StringTerm.upshift(t, amount, ~from))
  | SExpV(j) => SExpV(SExp.upshift(j, amount, ~from))
  }

let parse = (str: string, ~scope: array<StringTerm.meta>, ~gen=?) => {
  StringTerm.parse(str, ~scope, ~gen?)->Result.flatMap(((t, str)) =>
    SExp.parse(str, ~scope)->Result.map(((j, str)) => ((t, j), str))
  )
}

// HACK: this does work due to the hacky string-sexp conversion we have inplace with substitutions,
// but a different solution would be preferable
let placeSubstVal = (x: int, ~scope: array<string>) => StringV(StringTerm.place(x, ~scope))

let parseSubstVal = (str: string, ~scope: array<StringTerm.meta>, ~gen=?) => {
  switch StringTerm.parse(str, ~scope, ~gen?) {
  | Ok(t, str) => Ok(StringV(t), str)
  | Error(stringE) =>
    switch SExp.parse(str, ~scope) {
    | Ok(t, str) => Ok(SExpV(t), str)
    | Error(sExpE) =>
      Error(
        `string or sexp expected.\nstring parsing failed with error: ${stringE}\nsexp parsing failed with error: ${sExpE}`,
      )
    }
  }
}

let prettyPrint = ((t, j): t, ~scope: array<StringTerm.meta>) =>
  `${StringTerm.prettyPrint(t, ~scope)} ${SExp.prettyPrint(j, ~scope)}`
let prettyPrintSubstVal = (v: substCodom, ~scope: array<StringTerm.meta>) =>
  switch v {
  | StringV(t) => StringTerm.prettyPrint(t, ~scope)
  | SExpV(t) => SExp.prettyPrint(t, ~scope)
  }
