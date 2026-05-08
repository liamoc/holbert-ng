open Signatures
open Method

module MakeRewriteHOTerm = (
  HOTerm: HOTerm.S,
  Judgment: JUDGMENT with module Term := HOTerm and type t = HOTerm.t,
  Config: {
    let keyword: string
    let reversed: bool
  },
) => {
  module Term = HOTerm
  module Rule = Rule.Make(HOTerm, Judgment)
  module Context = Context(HOTerm, Judgment)
  module Results = MethodResults(HOTerm)

  let extractEqualityTermsFromJudgment = (judgment: Judgment.t): option<(HOTerm.t, HOTerm.t)> => {
    let term: HOTerm.t = judgment
    switch HOTerm.strip(term) {
    | (HOTerm.Symbol({name}), args) if HOTerm.isEqualityAtom(name) && Array.length(args) == 2 =>
      Some((args->Array.getUnsafe(0), args->Array.getUnsafe(1)))
    | _ => None
    }
  }

  let extractEqualityTerms = (rule: Rule.t): option<(HOTerm.t, HOTerm.t)> => {
    if Array.length(rule.premises) != 0 {
      None
    } else {
      extractEqualityTermsFromJudgment(rule.conclusion)
    }
  }

  let extractEqualityTermsFromBare = (rule: Rule.bare): option<(HOTerm.t, HOTerm.t)> => {
    if Array.length(rule.premises) != 0 {
      None
    } else {
      extractEqualityTermsFromJudgment(rule.conclusion)
    }
  }

  let isEqualityRule = (rule: Rule.t): bool => {
    extractEqualityTerms(rule)->Option.isSome
  }

  type t<'a> = {
    equalityName: string,
    instantiation: array<Term.t>,
    subgoal: 'a,
  }

  let keywords = [Config.keyword]

  let map = (it: t<'a>, f) => {
    {
      equalityName: it.equalityName,
      instantiation: it.instantiation,
      subgoal: f(it.subgoal),
    }
  }

  let substitute = (it: t<'a>, subst: Term.subst) => {
    {
      equalityName: it.equalityName,
      instantiation: it.instantiation->Array.map(t => t->Judgment.substitute(subst)),
      subgoal: it.subgoal,
    }
  }

  let prettyPrint = (
    it: t<'a>,
    ~scope,
    ~indentation=0,
    ~subprinter: ('a, ~scope: array<HOTerm.meta>, ~indentation: int=?) => string,
  ) => {
    let ind = String.repeat(" ", indentation)
    let args = it.instantiation->Array.map(t => Term.prettyPrint(t, ~scope))
    let argsStr = if Array.length(args) > 0 {
      " " ++ Array.join(args, " ")
    } else {
      ""
    }
    `${ind}${Config.keyword} (${it.equalityName}${argsStr}) {\n`
    ->String.concat(subprinter(it.subgoal, ~scope, ~indentation=indentation + 2))
    ->String.concat("\n")
    ->String.concat(ind)
    ->String.concat("}")
  }

  exception InternalParseError(string)

  let parse = (input, ~keyword as _, ~scope, ~gen, ~subparser) => {
    let cur = ref(String.trim(input))

    if cur.contents->String.get(0) == Some("(") {
      switch Rule.parseRuleName(cur.contents->String.sliceToEnd(~start=1)) {
      | Error(e) => Error(e)
      | Ok((equalityName, rest)) => {
          cur := rest
          let instantiation = []
          let it = ref(Error(""))
          while {
            it := Term.parse(cur.contents, ~scope, ~gen)
            it.contents->Result.isOk
          } {
            let (val, rest) = it.contents->Result.getExn
            Array.push(instantiation, val)
            cur := String.trim(rest)
          }
          if cur.contents->String.get(0) == Some(")") {
            cur := String.trim(cur.contents->String.sliceToEnd(~start=1))

            if cur.contents->String.get(0) == Some("{") {
              cur := String.trim(String.sliceToEnd(cur.contents, ~start=1))

              try {
                switch subparser(cur.contents, ~scope, ~gen) {
                | Error(e) => throw(InternalParseError(e))
                | Ok((subgoal, rest2)) => {
                    cur := String.trim(rest2)

                    if cur.contents->String.get(0) == Some("}") {
                      cur := String.trim(String.sliceToEnd(cur.contents, ~start=1))
                      Ok(({equalityName, instantiation, subgoal}, cur.contents))
                    } else {
                      Error("Expected } after subgoal")
                    }
                  }
                }
              } catch {
              | InternalParseError(e) => Error(e)
              }
            } else {
              Error("Expected { after equality instantiation")
            }
          } else {
            Error(") or term expected")
          }
        }
      }
    } else {
      Error("Expected (")
    }
  }

  let rewriteJudgmentTerms = (
    judgment: Judgment.t,
    from: HOTerm.t,
    to: HOTerm.t,
    ~gen: option<HOTerm.gen>,
  ): (HOTerm.subst, Judgment.t) => {
    let subst: ref<HOTerm.subst> = ref(HOTerm.makeSubst())
    let j = Judgment.mapTerms(judgment, term => {
      let (subst', newTerm) = HOTerm.rewrite(term, from, to, ~subst=subst.contents, ~gen)
      subst := subst'
      newTerm
    })
    (subst.contents, j)
  }

  let apply = (ctx: Context.t, j: Judgment.t, gen: HOTerm.gen, f: Rule.t => 'a) => {
    let ret: Dict.t<(t<'a>, Term.subst)> = Dict.make()

    ctx
    ->Context.facts
    ->Dict.forEachWithKey((eqRule, name) => {
      if isEqualityRule(eqRule) {
        let insts = eqRule->Rule.genSchemaInsts(gen, ~scope=ctx.fixes)
        let instantiatedRule = eqRule->Rule.instantiate(insts)

        switch extractEqualityTermsFromBare(instantiatedRule) {
        | Some((lhs, rhs)) => {
            let (from, to) = if Config.reversed {
              (rhs, lhs)
            } else {
              (lhs, rhs)
            }

            let (subst, rewrittenGoal) = rewriteJudgmentTerms(j, from, to, ~gen=Some(gen))

            // If the rewritten one is the same as the original, for example rewriting with reflexivity, skip it
            if (
              !Judgment.equivalent(
                j->Judgment.substitute(subst)->Judgment.reduce,
                rewrittenGoal->Judgment.substitute(subst)->Judgment.reduce,
              )
            ) {
              let rewrittenRule: Rule.t = {
                vars: [],
                premises: [],
                conclusion: rewrittenGoal,
              }
              let method = {
                equalityName: name,
                instantiation: insts,
                subgoal: f(rewrittenRule),
              }
              ret->Dict.set(`${Config.keyword} ${name}`, (method, subst))
            }
          }
        | None => ()
        }
      }
    })

    ret->Dict.toArray->Array.map(((s, (a, b))) => Results.Action(s, a, b))
  }

  let check = (it: t<'a>, ctx: Context.t, goal: Judgment.t, f: ('a, Rule.t) => 'b) => {
    switch ctx->Context.facts->Dict.get(it.equalityName) {
    | None => Error(`Cannot find equality '${it.equalityName}'`)
    | Some(eqRule) if !isEqualityRule(eqRule) =>
      Error(`'${it.equalityName}' is not a valid equality (has premises)`)
    | Some(eqRule) if Array.length(eqRule.vars) != Array.length(it.instantiation) =>
      Error(`Incorrect number of instantiation arguments for '${it.equalityName}'`)
    | Some(eqRule) => {
        let instantiatedRule = Rule.instantiate(eqRule, it.instantiation)

        switch extractEqualityTermsFromBare(instantiatedRule) {
        | None =>
          Error(`Cannot extract equality from '${it.equalityName}' - not in expected equality form`)
        | Some((lhs, rhs)) => {
            let (from, to) = if Config.reversed {
              (rhs, lhs)
            } else {
              (lhs, rhs)
            }

            // TODO: what gen to use?
            let (_, rewrittenGoal) = rewriteJudgmentTerms(goal, from, to, ~gen=None)

            let rewrittenRule: Rule.t = {
              vars: [],
              premises: [],
              conclusion: rewrittenGoal,
            }

            Ok({
              equalityName: it.equalityName,
              instantiation: it.instantiation,
              subgoal: f(it.subgoal, rewrittenRule),
            })
          }
        }
      }
    }
  }
}

module Rewrite = (
  HOTerm: HOTerm.S,
  Judgment: JUDGMENT with module Term := HOTerm and type t = HOTerm.t,
) => MakeRewriteHOTerm(
  HOTerm,
  Judgment,
  {
    let keyword = "rewrite"
    let reversed = false
  },
)

module RewriteReverse = (
  HOTerm: HOTerm.S,
  Judgment: JUDGMENT with module Term := HOTerm and type t = HOTerm.t,
) => MakeRewriteHOTerm(
  HOTerm,
  Judgment,
  {
    let keyword = "rewrite_reverse"
    let reversed = true
  },
)

module ConstructorNeq = (
  HOTerm: HOTerm.S,
  Judgment: JUDGMENT with module Term := HOTerm and type t = HOTerm.t,
) => {
  module Term = HOTerm
  module Rule = Rule.Make(HOTerm, Judgment)
  module Context = Context(HOTerm, Judgment)
  module Results = MethodResults(HOTerm)
  type t<'a> = unit

  type constructorHead = {name: string, args: array<HOTerm.t>}

  let constructorHead = (term: HOTerm.t): option<constructorHead> => {
    let (head, args) = HOTerm.strip(term)
    switch head {
    | HOTerm.Symbol({name: AtomBase.AnyValue(Symbolic.Base.Tag, name), constructor: true}) =>
      Some({name, args})
    | _ => None
    }
  }

  type constructorComparison = {lhs: constructorHead, rhs: constructorHead, negated: bool}

  let extractConstructorEqualityFrom = (term: HOTerm.t): option<(
    constructorHead,
    constructorHead,
  )> => {
    let (head, args) = HOTerm.strip(term)
    switch head {
    | HOTerm.Symbol({name}) if HOTerm.isEqualityAtom(name) && Array.length(args) == 2 =>
      switch (
        constructorHead(args->Array.getUnsafe(0)),
        constructorHead(args->Array.getUnsafe(1)),
      ) {
      | (Some(lhs), Some(rhs)) => Some((lhs, rhs))
      | _ => None
      }
    | _ => None
    }
  }

  let extractConstructorEquality = (judgment: Judgment.t): option<constructorComparison> => {
    let reduced = judgment->Judgment.reduce
    switch extractConstructorEqualityFrom(reduced) {
    | Some((lhs, rhs)) => Some({lhs, rhs, negated: false})
    | None =>
      switch HOTerm.strip(reduced) {
      | (HOTerm.Symbol({name: AtomBase.AnyValue(Symbolic.Base.Tag, "not")}), [inner]) =>
        extractConstructorEqualityFrom(inner)->Option.map(((lhs, rhs)) => {lhs, rhs, negated: true})
      | _ => None
      }
    }
  }

  let keywords = ["constructor_neq"]

  let map = (it: t<'a>, _f: 'a => 'b): t<'b> => it

  let substitute = (it: t<'a>, _subst: Term.subst) => it

  let prettyPrint = (_it: t<'a>, ~scope as _, ~indentation=0, ~subprinter as _) =>
    String.repeat(" ", indentation)->String.concat("constructor_neq")

  let parse = (input, ~keyword as _, ~scope as _, ~gen as _, ~subparser as _) => Ok((
    (),
    String.trim(input),
  ))

  let apply = (_ctx: Context.t, j: Judgment.t, _gen: HOTerm.gen, _f: Rule.t => 'a) => {
    let ret = Dict.make()
    switch extractConstructorEquality(j) {
    | Some({lhs: {name: lhs}, rhs: {name: rhs}}) if lhs != rhs =>
      ret->Dict.set(`constructor_neq ${lhs} ${rhs}`, ((), HOTerm.makeSubst()))
    | _ => ()
    }
    ret->Dict.toArray->Array.map(((s, (a, b))) => Results.Action(s, a, b))
  }

  let check = (_it: t<'a>, _ctx: Context.t, goal: Judgment.t, _f: ('a, Rule.t) => 'b) =>
    switch extractConstructorEquality(goal) {
    | Some({lhs: {name: lhs}, rhs: {name: rhs}}) =>
      if lhs == rhs {
        Error("constructor_neq expects different constructor symbols")
      } else {
        Ok()
      }
    | None => Error("constructor_neq applies only to equalities between constructors")
    }
}

module ConstructorInj = (
  HOTerm: HOTerm.S,
  Judgment: JUDGMENT with module Term := HOTerm and type t = HOTerm.t,
) => {
  module Rule = Rule.Make(HOTerm, Judgment)
  module Context = Context(HOTerm, Judgment)
  module Results = MethodResults(HOTerm)
  // we need to define this to workaround a type error for map
  type inner = {
    source: string,
    argIndex: int,
  }
  type t<'a> = inner

  let keywords = ["constructor_inj"]

  let map = (it: t<'a>, _f: 'a => 'b): t<'b> => it

  let substitute = (it: t<'a>, _subst: HOTerm.subst) => it

  let prettyPrint = (it: t<'a>, ~scope as _, ~indentation=0, ~subprinter as _) =>
    String.repeat(" ", indentation)->String.concat(
      `constructor_inj ${it.source} ${Int.toString(it.argIndex)}`,
    )

  let parseIntPrefix = (input: string): option<(int, string)> => {
    let cur = input->String.trimStart
    let re = RegExp.fromStringWithFlags("^(-?[0-9]+)", ~flags="y")
    switch re->RegExp.exec(cur) {
    | None => None
    | Some(res) =>
      switch RegExp.Result.matches(res) {
      | [n] =>
        Some((
          n->Int.fromString->Option.getExn,
          cur->String.sliceToEnd(~start=RegExp.lastIndex(re)),
        ))
      | _ => None
      }
    }
  }

  let parse = (input, ~keyword as _, ~scope as _, ~gen as _, ~subparser as _) => {
    let cur = String.trim(input)
    switch Rule.parseRuleName(cur) {
    | Error(e) => Error(e)
    | Ok((source, rest)) =>
      let rest = String.trim(rest)
      switch parseIntPrefix(rest) {
      | Some((argIndex, remainder)) => Ok(({source, argIndex}, remainder->String.trim))
      | None => Ok(({source, argIndex: 0}, rest))
      }
    }
  }

  let extractConstructorEquality = (term: HOTerm.t): option<(
    string,
    array<HOTerm.t>,
    array<HOTerm.t>,
  )> => {
    let (_head, args) = term->HOTerm.strip
    if Array.length(args) != 2 {
      None
    } else {
      let (lHead, lArgs) = args->Array.getUnsafe(0)->HOTerm.strip
      let (rHead, rArgs) = args->Array.getUnsafe(1)->HOTerm.strip
      switch (lHead, rHead) {
      | (
          HOTerm.Symbol({name: AtomBase.AnyValue(Symbolic.Base.Tag, ln), constructor: true}),
          HOTerm.Symbol({name: AtomBase.AnyValue(Symbolic.Base.Tag, rn), constructor: true}),
        ) if ln == rn && lArgs->Array.length == rArgs->Array.length =>
        Some((ln, lArgs, rArgs))
      | _ => None
      }
    }
  }

  let apply = (ctx: Context.t, j: Judgment.t, _gen: HOTerm.gen, _f: Rule.t => 'a) => {
    let ret = Dict.make()
    switch j->Judgment.reduce->HOTerm.strip {
    | (HOTerm.Symbol({name}), [lhs, rhs]) if HOTerm.isEqualityAtom(name) =>
      ctx
      ->Context.facts
      ->Dict.forEachWithKey((fact, name) => {
        switch extractConstructorEquality(fact.conclusion) {
        | Some((_cName, lArgs, rArgs)) =>
          Belt.Array.zip(lArgs, rArgs)->Array.forEachWithIndex(((la, ra), idx) =>
            if HOTerm.equivalent(la, lhs) && HOTerm.equivalent(ra, rhs) {
              ret->Dict.set(
                `constructor_inj ${name} ${Int.toString(idx)}`,
                ({source: name, argIndex: idx}, HOTerm.makeSubst()),
              )
            }
          )
        | None => ()
        }
      })
    | _ => ()
    }
    ret->Dict.toArray->Array.map(((s, (a, b))) => Results.Action(s, a, b))
  }

  let check = (it: t<'a>, ctx: Context.t, goal: Judgment.t, _f: ('a, Rule.t) => 'b) => {
    switch (ctx->Context.facts->Dict.get(it.source), goal->Judgment.reduce->HOTerm.strip) {
    | (None, _) => Error(`Cannot find equality '${it.source}'`)
    | (Some(fact), (HOTerm.Symbol({name}), [lhs, rhs])) if HOTerm.isEqualityAtom(name) =>
      switch extractConstructorEquality(fact.conclusion) {
      | None => Error(`'${it.source}' is not a constructor equality`)
      | Some((_cName, lArgs, rArgs)) =>
        if it.argIndex < 0 || it.argIndex >= Array.length(lArgs) {
          Error("constructor_inj index out of range")
        } else {
          let la = lArgs->Array.getUnsafe(it.argIndex)
          let ra = rArgs->Array.getUnsafe(it.argIndex)
          if HOTerm.equivalent(la, lhs) && HOTerm.equivalent(ra, rhs) {
            Ok({source: it.source, argIndex: it.argIndex})
          } else {
            Error("constructor_inj target does not match goal")
          }
        }
      }
    | (_, _) => Error("constructor_inj applies to equality goals")
    }
  }
}
