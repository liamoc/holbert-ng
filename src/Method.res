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
      instantiation: it.instantiation->Array.map(t => t->Judgment.substituteSubstVal(subst)),
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
    let args = it.instantiation->Array.map(t => Judgment.prettyPrintSubstVal(t, ~scope))
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
            it := Judgment.parseSubstVal(cur.contents, ~scope, ~gen)
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
      let insts = rule->Rule.schematise(gen, ~scope=ctx.fixes)
      let res = rule->Rule.instantiate(insts)
      let substs = Judgment.unify(res.conclusion, j, ~gen)
      substs->Seq.forEach(subst => {
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
    let subgoalsStr =
      it.subgoals
      ->Array.map(s => subprinter(s, ~scope, ~indentation=indentation + 2))
      ->Array.join(newline)
    `elim (${it.ruleName} ${it.elimName}) {${subgoalsSpacer}${subgoalsStr}}`
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
      instantiation: it.instantiation->Array.map(t => t->Judgment.substituteSubstVal(subst)),
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
        Rule.parseRuleName(cur.contents)->Result.flatMap(((elimName, _rest)) => {
          let instantiation = []
          let it = ref(Error(""))
          while {
            it := Judgment.parseSubstVal(cur.contents, ~scope, ~gen)
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
                  Ok(({ruleName, elimName, instantiation, subgoals}, cur.contents))
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
            subgoals: Belt.Array.zipBy(it.subgoals, premises, f),
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
        r.premises->Array.length > 0 && (r.premises[0]->Option.getExn).premises->Array.length == 0
      )
    let possibleElims =
      ctx.facts->Dict.toArray->Array.filter(((_, r)) => r.premises->Array.length == 0)
    possibleRules->Array.forEach(((ruleName, rule)) => {
      possibleElims->Array.forEach(((elimName, elim)) => {
        let ruleInsts = rule->Rule.schematise(gen, ~scope=ctx.fixes)
        let elimInsts = elim->Rule.schematise(gen, ~scope=ctx.fixes)
        let (rule', elim) = (rule->Rule.instantiate(ruleInsts), elim->Rule.instantiate(elimInsts))
        Judgment.unify((rule.premises[0]->Option.getExn).conclusion, elim.conclusion)->Seq.forEach(
          elimSub => {
            let rule'' = rule'->Rule.substituteBare(elimSub)
            Judgment.unify(rule''.conclusion, j, ~gen)->Seq.forEach(
              ruleSub => {
                let subst = Judgment.mergeSubsts(elimSub, ruleSub)
                let new = {
                  ruleName,
                  elimName,
                  instantiation: ruleInsts,
                  subgoals: rule.premises->Array.map(f),
                }
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
