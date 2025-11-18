open Signatures
open Util
module Context = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module Rule = Rule.Make(Term, Judgment)
  type t = {
    fixes: array<Term.meta>,
    facts: Dict.t<Rule.t>,
  }
}

module type PROOF_METHOD = {
  module Term: TERM
  module Judgment: JUDGMENT with module Term := Term
  module Rule: module type of Rule.Make(Term, Judgment)
  module Context: module type of Context(Term, Judgment)
  type t<'a>
  let keywords: array<string>
  let substitute: (t<'a>, Judgment.subst) => t<'a>
  let check: (t<'a>, Context.t, Judgment.t, ('a, Rule.t) => 'b) => result<t<'b>, string>
  let apply: (Context.t, Judgment.t, Term.gen, Rule.t => 'a) => Dict.t<(t<'a>, Judgment.subst)>
  let map: (t<'a>, 'a => 'b) => t<'b>
  let parse: (
    string,
    ~keyword: string,
    ~scope: array<Term.meta>,
    ~gen: Term.gen,
    ~subparser: (string, ~scope: array<Term.meta>, ~gen: Term.gen) => result<('a, string), string>,
  ) => result<(t<'a>, string), string>
  let prettyPrint: (
    t<'a>,
    ~scope: array<Term.meta>,
    ~indentation: int=?,
    ~subprinter: ('a, ~scope: array<Term.meta>, ~indentation: int=?) => string,
  ) => string
}

let seqSizeLimit = 100

module Derivation = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module Rule = Rule.Make(Term, Judgment)
  module Context = Context(Term, Judgment)
  type t<'a> = {
    ruleName: string,
    instantiation: array<Judgment.substCodom>,
    subgoals: array<'a>,
  }
  let map = (it: t<'a>, f) => {
    {
      ruleName: it.ruleName,
      instantiation: it.instantiation,
      subgoals: it.subgoals->Array.map(f),
    }
  }
  let substitute = (it: t<'a>, subst: Judgment.subst) => {
    {
      ruleName: it.ruleName,
      instantiation: it.instantiation->Array.map(t => t->Judgment.substituteSubstCodom(subst)),
      subgoals: it.subgoals,
    }
  }
  exception InternalParseError(string)
  let keywords = ["by"]
  let prettyPrint = (
    it: t<'a>,
    ~scope,
    ~indentation=0,
    ~subprinter: ('a, ~scope: array<Term.meta>, ~indentation: int=?) => string,
  ) => {
    let args = it.instantiation->Array.map(t => Judgment.prettyPrintSubstCodom(t, ~scope))
    "by ("
    ->String.concat(Array.join([it.ruleName]->Array.concat(args), " "))
    ->String.concat(") {")
    ->String.concat(
      if Array.length(it.subgoals) > 0 {
        newline
      } else {
        ""
      },
    )
    ->String.concat(
      it.subgoals
      ->Array.map(s => subprinter(s, ~scope, ~indentation=indentation + 2))
      ->Array.join(newline),
    )
    ->String.concat("}")
  }
  let parse = (input, ~keyword as _, ~scope, ~gen, ~subparser) => {
    let cur = ref(String.trim(input))
    if cur.contents->String.get(0) == Some("(") {
      switch Rule.parseRuleName(cur.contents->String.sliceToEnd(~start=1)) {
      | Ok((ruleName, rest)) => {
          cur := rest
          let instantiation = []
          let it = ref(Error(""))
          while {
            it := Judgment.parseSubstCodom(cur.contents, ~scope, ~gen)
            it.contents->Result.isOk
          } {
            let (val, rest) = it.contents->Result.getExn
            Array.push(instantiation, val)
            cur := String.trim(rest)
          }
          if cur.contents->String.get(0) == Some(")") {
            cur := String.trim(cur.contents->String.sliceToEnd(~start=1))
            let subgoals = []
            if cur.contents->String.get(0) == Some("{") {
              cur := String.trim(cur.contents->String.sliceToEnd(~start=1))
              try {
                while cur.contents->String.get(0) != Some("}") {
                  switch subparser(cur.contents, ~scope, ~gen) {
                  | Ok((sg, rest)) => {
                      Array.push(subgoals, sg)
                      cur := String.trim(rest)
                    }
                  | Error(e) => raise(InternalParseError(e))
                  }
                }
                if cur.contents->String.get(0) == Some("}") {
                  cur := String.trim(cur.contents->String.sliceToEnd(~start=1))
                  Ok(({ruleName, instantiation, subgoals}, cur.contents))
                } else {
                  Error("} or subgoal proof expected")
                }
              } catch {
              | InternalParseError(e) => Error(e)
              }
            } else {
              Error("{ expected")
            }
          } else {
            Error(") or term expected")
          }
        }
      | Error(e) => Error(e)
      }
    } else {
      Error("Expected (")
    }
  }
  let apply = (ctx: Context.t, j: Judgment.t, gen: Term.gen, f: Rule.t => 'a) => {
    let ret = Dict.make()
    ctx.facts->Dict.forEachWithKey((rule, key) => {
      let insts = rule->Rule.genSchemaInsts(gen, ~scope=ctx.fixes)
      let res = rule->Rule.instantiate(insts)
      let substs = Judgment.unify(res.conclusion, j, ~gen)
      substs
      ->Seq.take(seqSizeLimit)
      ->Seq.forEach(subst => {
        let new = {
          ruleName: key,
          instantiation: insts,
          subgoals: res.premises->Array.map(f),
        }
        ret->Dict.set("intro " ++ key, (new, subst))
      })
    })
    ret
  }
  let check = (it: t<'a>, ctx: Context.t, j: Judgment.t, f: ('a, Rule.t) => 'b) => {
    switch ctx.facts->Dict.get(it.ruleName) {
    | None => Error("Cannot find rule '"->String.concat(it.ruleName)->String.concat("'"))
    | Some(rule) if Array.length(rule.vars) == Array.length(it.instantiation) => {
        let {premises, conclusion} = Rule.instantiate(rule, it.instantiation)
        if Judgment.equivalent(conclusion, j) {
          if Array.length(it.subgoals) == Array.length(premises) {
            Ok({
              ruleName: it.ruleName,
              instantiation: it.instantiation,
              subgoals: Belt.Array.zipBy(it.subgoals, premises, f),
            })
          } else {
            Error("Incorrect number of subgoals")
          }
        } else {
          let concString = Judgment.prettyPrint(conclusion, ~scope=ctx.fixes)
          let goalString = Judgment.prettyPrint(j, ~scope=ctx.fixes)
          Error(
            "Conclusion of rule '"
            ->String.concat(concString)
            ->String.concat("' doesn't match goal '")
            ->String.concat(goalString)
            ->String.concat("'"),
          )
        }
      }
    | _ => Error("Incorrect number of binders")
    }
  }
  let updateAtKey = (it: t<'a>, key: int, f: 'a => 'a) => {
    let newsgs = it.subgoals->Array.copy
    newsgs->Array.set(key, f(newsgs[key]->Option.getExn))
    {...it, subgoals: newsgs}
  }
}

module Elimination = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module Rule = Rule.Make(Term, Judgment)
  module Context = Context(Term, Judgment)
  type t<'a> = {
    ruleName: string,
    elimName: string,
    instantiation: array<Judgment.substCodom>,
    subgoals: array<'a>,
  }
  exception InternalParseError(string)
  let keywords = ["elim"]
  let prettyPrint = (
    it: t<'a>,
    ~scope,
    ~indentation=0,
    ~subprinter: ('a, ~scope: array<Term.meta>, ~indentation: int=?) => string,
  ) => {
    let subgoalsSpacer = if Array.length(it.subgoals) > 0 {
      newline
    } else {
      ""
    }
    let instantiation = Array.join(
      it.instantiation->Array.map(t => Judgment.prettyPrintSubstCodom(t, ~scope)),
      " ",
    )
    let subgoalsStr =
      it.subgoals
      ->Array.map(s => subprinter(s, ~scope, ~indentation=indentation + 2))
      ->Array.join(newline)
    `elim (${it.ruleName} ${it.elimName} ${instantiation}) {${subgoalsSpacer}${subgoalsStr}}`
  }

  let map = (it: t<'a>, f) => {
    {
      ruleName: it.ruleName,
      elimName: it.elimName,
      instantiation: it.instantiation,
      subgoals: it.subgoals->Array.map(f),
    }
  }

  let substitute = (it: t<'a>, subst: Judgment.subst) => {
    {
      ruleName: it.ruleName,
      elimName: it.elimName,
      instantiation: it.instantiation->Array.map(t => t->Judgment.substituteSubstCodom(subst)),
      subgoals: it.subgoals,
    }
  }

  let parse = (input, ~keyword as _, ~scope, ~gen, ~subparser) => {
    let cur = ref(String.trim(input))
    if cur.contents->String.get(0) == Some("(") {
      Rule.parseRuleName(cur.contents->String.sliceToEnd(~start=1))->Result.flatMap(((
        ruleName,
        rest,
      )) => {
        cur := rest
        Rule.parseRuleName(cur.contents)->Result.flatMap(((elimName, rest)) => {
          cur := rest
          let instantiation = []
          let it = ref(Error(""))
          while {
            it := Judgment.parseSubstCodom(cur.contents, ~scope, ~gen)
            it.contents->Result.isOk
          } {
            let (val, rest) = it.contents->Result.getExn
            Array.push(instantiation, val)
            cur := String.trim(rest)
          }
          if cur.contents->String.get(0) == Some(")") {
            cur := String.trim(cur.contents->String.sliceToEnd(~start=1))
            let subgoals = []
            if cur.contents->String.get(0) == Some("{") {
              cur := String.trim(cur.contents->String.sliceToEnd(~start=1))
              try {
                while cur.contents->String.get(0) != Some("}") {
                  switch subparser(cur.contents, ~scope, ~gen) {
                  | Ok((sg, rest)) => {
                      Array.push(subgoals, sg)
                      cur := String.trim(rest)
                    }
                  | Error(e) => raise(InternalParseError(e))
                  }
                }
                if cur.contents->String.get(0) == Some("}") {
                  cur := String.trim(cur.contents->String.sliceToEnd(~start=1))
                  let res = {ruleName, elimName, instantiation, subgoals}
                  Console.log(("parsed elim", res))
                  Ok((res, cur.contents))
                } else {
                  Error("} or subgoal proof expected")
                }
              } catch {
              | InternalParseError(e) => Error(e)
              }
            } else {
              Error("{ expected")
            }
          } else {
            Error(") or term expected")
          }
        })
      })
    } else {
      Error("Expected (")
    }
  }

  let check = (it: t<'a>, ctx: Context.t, j: Judgment.t, f: ('a, Rule.t) => 'b) => {
    switch (ctx.facts->Dict.get(it.ruleName), ctx.facts->Dict.get(it.elimName)) {
    | (None, _) => Error(`Cannot find rule '${it.ruleName}'`)
    | (_, None) => Error(`Cannot find elimination fact '${it.elimName}'`)
    | (Some(rule), Some(elim)) if rule.premises->Array.length > 0 => {
        let {premises, conclusion} = Rule.instantiate(rule, it.instantiation)
        let elimPremise = premises[0]->Option.getExn
        let remainingPremises = premises->Array.sliceToEnd(~start=1)
        if elimPremise.premises->Array.length > 0 {
          Error(`Premise to eliminate in rule ${it.ruleName} has non-empty premises`)
        } else if elim.premises->Array.length > 0 {
          Error(`Elimination motive (?) ${it.elimName} has non-empty premises`)
        } else if !Judgment.equivalent(elimPremise.conclusion, elim.conclusion) {
          Error(`Premise to eliminate and elimination motive (?) ${it.elimName} do not match`)
        } else if !Judgment.equivalent(conclusion, j) {
          let concString = Judgment.prettyPrint(conclusion, ~scope=ctx.fixes)
          let goalString = Judgment.prettyPrint(j, ~scope=ctx.fixes)
          Error(`Conclusion of rule '${concString}' doesn't match goal '${goalString}'`)
        } else if Array.length(it.subgoals) != Array.length(remainingPremises) {
          let subgoalsRem = Array.length(it.subgoals)->Int.toString
          let premsRem = Array.length(remainingPremises)->Int.toString
          Error(
            `Number of subgoals (${subgoalsRem}) doesn't match rule ${it.ruleName}'s remaining number (${premsRem})`,
          )
        } else {
          Ok({
            ruleName: it.ruleName,
            elimName: it.elimName,
            instantiation: it.instantiation,
            subgoals: Belt.Array.zipBy(it.subgoals, remainingPremises, f),
          })
        }
      }
    | (Some(_), Some(_)) => Error(`Rule ${it.ruleName} doesn't have any premises`)
    }
  }

  let updateAtKey = (it: t<'a>, key: int, f: 'a => 'a) => {
    let newsgs = it.subgoals->Array.copy
    newsgs->Array.set(key, f(newsgs[key]->Option.getExn))
    {...it, subgoals: newsgs}
  }

  let apply = (ctx: Context.t, j: Judgment.t, gen: Term.gen, f: Rule.t => 'a) => {
    let ret = Dict.make()
    let possibleRules =
      ctx.facts
      ->Dict.toArray
      ->Array.filter(((_, r)) =>
        r.premises->Array.length > 0 && {
            let fst = r.premises[0]->Option.getExn
            fst.premises->Array.length == 0 && fst.vars->Array.length == 0
          }
      )
    let possibleElims =
      ctx.facts
      ->Dict.toArray
      ->Array.filter(((_, r)) => r.premises->Array.length == 0 && r.vars->Array.length == 0)
    possibleRules->Array.forEach(((ruleName, rule)) => {
      possibleElims->Array.forEach(((elimName, elim)) => {
        let ruleInsts = rule->Rule.genSchemaInsts(gen, ~scope=ctx.fixes)
        let rule' = rule->Rule.instantiate(ruleInsts)
        Judgment.unify((rule'.premises[0]->Option.getExn).conclusion, elim.conclusion, ~gen)
        ->Seq.take(seqSizeLimit)
        ->Seq.forEach(
          elimSub => {
            let rule'' = rule'->Rule.substituteBare(elimSub)
            Judgment.unify(rule''.conclusion, j, ~gen)
            ->Seq.take(seqSizeLimit)
            ->Seq.forEach(
              ruleSub => {
                let new = {
                  ruleName,
                  elimName,
                  instantiation: ruleInsts,
                  subgoals: rule.premises->Array.sliceToEnd(~start=1)->Array.map(f),
                }
                let subst = Judgment.mergeSubsts(elimSub, ruleSub)
                ret->Dict.set(`elim ${ruleName} with ${elimName}`, (new, subst))
              },
            )
          },
        )
      })
    })
    ret
  }
}

module Lemma = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module Rule = Rule.Make(Term, Judgment)
  module Context = Context(Term, Judgment)
  type t<'a> = {
    rule: Rule.t,
    proof: 'a,
    show: 'a,
  }
  let map = (it: t<'a>, f) => {
    {
      rule: it.rule,
      proof: f(it.proof),
      show: f(it.show),
    }
  }
  let substitute = (it: t<'a>, subst: Judgment.subst) => {
    {
      rule: it.rule->Rule.substitute(subst),
      proof: it.proof,
      show: it.show,
    }
  }
  let keywords = ["have"]
  let prettyPrint = (
    it: t<'a>,
    ~scope,
    ~indentation=0,
    ~subprinter: ('a, ~scope: array<Term.meta>, ~indentation: int=?) => string,
  ) => {
    "have "
    ->String.concat(Rule.prettyPrintInline(it.rule, ~scope))
    ->String.concat(newline)
    ->String.concat(subprinter(it.proof, ~scope, ~indentation))
    ->String.concat(newline)
    ->String.concat(subprinter(it.show, ~scope, ~indentation))
  }
  let parse = (input, ~keyword as _, ~scope, ~gen, ~subparser) => {
    //todo add toplevel
    switch Rule.parseInner(input, ~scope, ~gen) {
    | Ok((rule, rest)) =>
      switch subparser(rest, ~scope, ~gen) {
      | Ok((proof, rest')) =>
        switch String.trim(rest')->subparser(~scope, ~gen) {
        | Ok((show, rest'')) => Ok({rule, proof, show}, rest'')
        | Error(e) => Error(e)
        }
      | Error(e) => Error(e)
      }
    | Error(e) => Error(e)
    }
  }
  let apply = (_ctx: Context.t, _j: Judgment.t, _gen: Term.gen, _f: Rule.t => 'a) => {
    Dict.make()
  }
  let check = (it: t<'a>, _ctx: Context.t, j: Judgment.t, f: ('a, Rule.t) => 'b) => {
    let first = f(it.proof, it.rule)
    let second = f(it.show, {vars: [], premises: [it.rule], conclusion: j})
    Ok({rule: it.rule, proof: first, show: second})
  }
}
module Combine = (
  Term: TERM,
  Judgment: JUDGMENT with module Term := Term,
  Method1: PROOF_METHOD with module Term := Term and module Judgment := Judgment,
  Method2: PROOF_METHOD with module Term := Term and module Judgment := Judgment,
) => {
  module Rule = Rule.Make(Term, Judgment)
  module Context = Context(Term, Judgment)
  type t<'a> = First(Method1.t<'a>) | Second(Method2.t<'a>)
  let map = (it, f) =>
    switch it {
    | First(m) => First(Method1.map(m, f))
    | Second(m) => Second(Method2.map(m, f))
    }
  let substitute = (it, subst) =>
    switch it {
    | First(m) => First(Method1.substitute(m, subst))
    | Second(m) => Second(Method2.substitute(m, subst))
    }
  let keywords = Array.concat(Method1.keywords, Method2.keywords)
  let apply = (ctx: Context.t, j: Judgment.t, gen: Term.gen, f: Rule.t => 'a) => {
    let d1 = Method1.apply(ctx, j, gen, f)->Dict.mapValues(((m, s)) => (First(m), s))
    let d2 = Method2.apply(ctx, j, gen, f)->Dict.mapValues(((m, s)) => (Second(m), s))
    d1->Dict.assign(d2)
  }
  let check = (it, ctx, j, f) =>
    switch it {
    | First(m) => m->Method1.check(ctx, j, f)->Result.map(x => First(x))
    | Second(m) => m->Method2.check(ctx, j, f)->Result.map(x => Second(x))
    }
  let prettyPrint = (it: t<'a>, ~scope, ~indentation=0, ~subprinter) =>
    switch it {
    | First(m) => m->Method1.prettyPrint(~scope, ~indentation, ~subprinter)
    | Second(m) => m->Method2.prettyPrint(~scope, ~indentation, ~subprinter)
    }
  let parse = (input, ~keyword, ~scope, ~gen, ~subparser) => {
    if Method1.keywords->Array.indexOf(keyword) > -1 {
      Method1.parse(input, ~keyword, ~scope, ~gen, ~subparser)->Result.map(((x, r)) => (
        First(x),
        r,
      ))
    } else {
      Method2.parse(input, ~keyword, ~scope, ~gen, ~subparser)->Result.map(((x, r)) => (
        Second(x),
        r,
      ))
    }
  }
}

module MakeRewriteHOTerm = (
  Judgment: JUDGMENT with module Term := HOTerm and type subst = HOTerm.subst and type t = HOTerm.t,
  Config: {
    let keyword: string
    let reversed: bool
  },
) => {
  module Term = HOTerm
  module Rule = Rule.Make(HOTerm, Judgment)
  module Context = Context(HOTerm, Judgment)

  let extractEqualityTermsFromJudgment = (judgment: Judgment.t): option<(HOTerm.t, HOTerm.t)> => {
    let term: HOTerm.t = judgment
    switch HOTerm.strip(term) {
    | (HOTerm.Symbol({name: "="}), args) if Array.length(args) == 2 =>
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
    instantiation: array<Judgment.substCodom>,
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

  let substitute = (it: t<'a>, subst: Judgment.subst) => {
    {
      equalityName: it.equalityName,
      instantiation: it.instantiation->Array.map(t => t->Judgment.substituteSubstCodom(subst)),
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
    let args = it.instantiation->Array.map(t => Judgment.prettyPrintSubstCodom(t, ~scope))
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
            it := Judgment.parseSubstCodom(cur.contents, ~scope, ~gen)
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
                | Error(e) => raise(InternalParseError(e))
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
    let ret: Dict.t<(t<'a>, Judgment.subst)> = Dict.make()

    ctx.facts->Dict.forEachWithKey((eqRule, name) => {
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

    ret
  }

  let check = (it: t<'a>, ctx: Context.t, goal: Judgment.t, f: ('a, Rule.t) => 'b) => {
    switch ctx.facts->Dict.get(it.equalityName) {
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
  Judgment: JUDGMENT with module Term := HOTerm and type subst = HOTerm.subst and type t = HOTerm.t,
) => MakeRewriteHOTerm(
  Judgment,
  {
    let keyword = "rewrite"
    let reversed = false
  },
)

module RewriteReverse = (
  Judgment: JUDGMENT with module Term := HOTerm and type subst = HOTerm.subst and type t = HOTerm.t,
) => MakeRewriteHOTerm(
  Judgment,
  {
    let keyword = "rewrite_reverse"
    let reversed = true
  },
)
