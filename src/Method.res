open Signatures

module RuleRef = {

  type t = Local({index: int}) | Global({name: string})
  
  let prettyPrint = (id: t, ~assms: array<string>): string => {
    switch id {
    | Global({name}) => "--" ++ name
    | Local({index}) =>
      switch assms[index] {
      | None => "-" ++ Int.toString(index) // Fallback if out of bounds
      | Some(name) =>
        // Check if this name appears at any index strictly higher than `index`
        let isShadowed = assms->Array.reduceWithIndex(false, (acc, item, i) => {
          acc || (i > index && item == name)
        })

        if isShadowed {
          "-" ++ Int.toString(index)
        } else {
          name
        }
      }
    }
  }
  
  let parse = (input: string, ~assms: array<string>): result<(t,string),string> => {
    // Group 1: Local numeric index
    // Group 2: Global identifier
    // Group 3: Bare identifier
    // Group 4: Remaining string
    let pattern = %re("/^(?:-(\d+)(?![^()\[\]{}|\-\s])|--([^()\[\]{}|\-\s][^()\[\]{}|\s]*)|([^()\[\]{}|\-\s][^()\[\]{}|\s]*))(.*)$/s")
    
    switch RegExp.exec(pattern, input) {
    | None => Error("Syntax error: Invalid identifier format")
    | Some(result) =>
      let matches = RegExp.Result.matches(result)
      let rest = matches[3]->Option.getOr("")

      switch (matches[0], matches[1], matches[2]) {
      | (Some(numStr), _, _) =>{ Console.log(matches)
        switch Int.fromString(numStr) {
        | Some(n) => Ok((Local({index: n}), rest))
        | None => Error("Not that many local rules")
        }}

      | (_, Some(name), _) =>
        Ok((Global({name: name}), rest))

      | (_, _, Some(ident)) =>
        let highestIdx = assms->Array.reduceWithIndex(-1, (acc, item, idx) => {
          item == ident ? idx : acc
        })

        if highestIdx == -1 {
          Ok((Global({name: ident}), rest))
        } else {
          Ok((Local({index: highestIdx}), rest))
        }

      | _ => Error("Failed to match valid identifier token")
      }
    }
  }

}
module Context = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module Rule = Rule.Make(Term, Judgment)
  type t = {
    fixes: array<Term.meta>,
    localFacts: array<Rule.t>, 
    localFactNames: array<string>,
    globalFacts: Dict.t<Rule.t>,
  }
  
  let lookup = (ctx: t, r: RuleRef.t) =>
    switch r {
    | Local({index: i}) => ctx.localFacts[i] 
    | Global({name: i}) => ctx.globalFacts->Dict.get(i)
    }
  
  let facts = (ctx: t): array<(RuleRef.t, Rule.t)> => {
    let locals = ctx.localFacts->Array.mapWithIndex((rule, index) => {
      (RuleRef.Local({index: index}), rule)
    })
    let globals =
      ctx.globalFacts
      ->Dict.toArray
      ->Array.map(((name, rule)) => (RuleRef.Global({name: name}), rule))

    Array.concat(locals, globals)
  }
}

module MethodResults = (Term: TERM) => {
  type rec t<'a> =
    | Action(string, 'a, Term.subst)
    | Delay(string, unit => array<t<'a>>)
    | Group(string, array<t<'a>>)

  let rec map = (x: t<'a>, f: 'a => 'b) =>
    switch x {
    | Action(str, a, sub) => Action(str, f(a), sub)
    | Delay(str, g) => Delay(str, () => g()->Array.map(x => x->map(f)))
    | Group(str, gs) => Group(str, gs->Array.map(x => x->map(f)))
    }

  type attached<'a> = {
    goal: array<t<'a>>,
    assumptions: array<(int, array<t<'a>>)>,
  }

  let emptyAttached: () => attached<'a> = () => {goal: [], assumptions: []}

  let atGoal = (results: array<t<'a>>): attached<'a> => {goal: results, assumptions: []}
  let atAssumption = (index: int, results: array<t<'a>>): attached<'a> => {
    goal: [],
    assumptions: [(index, results)],
  }

  let combine = (a: attached<'a>, b: attached<'a>): attached<'a> => {
    let assumptions = Dict.make()
    Array.concat(a.assumptions, b.assumptions)->Array.forEach(((i, rs)) => {
      let key = Int.toString(i)
      let existing = assumptions->Dict.get(key)->Option.getOr([])
      assumptions->Dict.set(key, Array.concat(existing, rs))
    })
    {
      goal: Array.concat(a.goal, b.goal),
      assumptions:
        assumptions
        ->Dict.toArray
        ->Array.map(((k, rs)) => (Int.fromString(k)->Option.getExn, rs)),
    }
  }

  let mapAttached = (x: attached<'a>, f: 'a => 'b): attached<'b> => {
    goal: x.goal->Array.map(r => r->map(f)),
    assumptions: x.assumptions->Array.map(((i, rs)) => (i, rs->Array.map(r => r->map(f)))),
  }
}

module type PROOF_METHOD = {
  module Term: TERM
  module Judgment: JUDGMENT with module Term := Term
  module Rule: module type of Rule.Make(Term, Judgment)
  module Context: module type of Context(Term, Judgment)
  module Results: module type of MethodResults(Term)
  type t<'a>
  let keywords: array<string>
  let substitute: (t<'a>, Term.subst) => t<'a>
  let check: (t<'a>, Context.t, Judgment.t, ('a, Rule.t) => 'b) => result<t<'b>, string>
  let apply: (Context.t, Judgment.t, Term.gen, Rule.t => 'a) => Results.attached<t<'a>>
  let map: (t<'a>, 'a => 'b) => t<'b>
  type key
  let subproofs: t<'a> => array<(key,'a)>
  let setSubproof: (t<'a>, key, 'a) => t<'a>
  
  let parse: (
    string,
    ~keyword: string,
    ~grammar: Term.grammar,
    ~scope: array<Term.meta>,
    ~assms: array<string>,
    ~gen: Term.gen,
    ~subparser: (string, ~grammar: Term.grammar, ~scope: array<Term.meta>, ~assms: array<string>, ~gen: Term.gen) => result<('a, string), string>,
  ) => result<(t<'a>, string), string>
  let prettyPrint: (
    t<'a>,
    ~grammar: Term.grammar,
    ~scope: array<Term.meta>,
    ~assms: array<string>,
    ~indentation: int=?,
    ~subprinter: ('a, ~grammar: Term.grammar, ~scope: array<Term.meta>, ~assms: array<string>, ~indentation: int=?) => string,
  ) => string
}

let seqSizeLimit = 100
let newline = Util.newline

module Derivation = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module Rule = Rule.Make(Term, Judgment)
  module Context = Context(Term, Judgment)
  module Results = MethodResults(Term)
  type t<'a> = {
    ruleName: RuleRef.t,
    instantiation: array<Term.t>,
    subgoals: array<'a>,
  }
  type key = int
  let subproofs = it => it.subgoals -> Array.mapWithIndex((x,i) => (i,x))

  let setSubproof = (it, key, sg) => {
    let newsgs = it.subgoals->Array.copy
    newsgs->Array.set(key, sg)
    {...it, subgoals: newsgs}
  }
  

  let map = (it: t<'a>, f) => {
    {
      ruleName: it.ruleName,
      instantiation: it.instantiation,
      subgoals: it.subgoals->Array.map(f),
    }
  }
  let substitute = (it: t<'a>, subst: Term.subst) => {
    {
      ruleName: it.ruleName,
      instantiation: it.instantiation->Array.map(t => t->Term.substitute(subst)),
      subgoals: it.subgoals,
    }
  }
  exception InternalParseError(string)
  let keywords = ["by"]
  let prettyPrint = (it: t<'a>,~grammar,~scope,~assms: array<string>,~indentation=0,
    ~subprinter : ('a, ~grammar:Term.grammar, ~scope: array<Term.meta>, ~assms: array<string>, ~indentation: int=?) => string) => {
    let args = it.instantiation->Array.map(t => Term.prettyPrint(t, ~grammar, ~scope))
    "by ("
    ->String.concat(Array.join([RuleRef.prettyPrint(it.ruleName,~assms)]->Array.concat(args), " "))
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
      ->Array.map(s => subprinter(s, ~grammar, ~scope,~assms, ~indentation=indentation + 2))
      ->Array.join(newline),
    )
    ->String.concat("}")
  }
  
  let parse = (input, ~keyword as _, ~grammar, ~scope, ~assms, ~gen, ~subparser) => {
    let cur = ref(String.trim(input))
    if cur.contents->String.get(0) == Some("(") {
      switch RuleRef.parse(String.trim(cur.contents->String.sliceToEnd(~start=1)),~assms) {
      | Ok((ruleName, rest)) => {
          cur := rest
          let instantiation = []
          let it = ref(Error(""))
          while {
            it := Term.parse(cur.contents, ~grammar, ~scope, ~gen)
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
                  switch subparser(cur.contents, ~grammar,~scope, ~assms, ~gen) {
                  | Ok((sg, rest)) => {
                      Array.push(subgoals, sg)
                      cur := String.trim(rest)
                    }
                  | Error(e) => throw(InternalParseError(e))
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
    ctx
    ->Context.facts
    ->Array.filterMap(((key, rule)) => {
      let keyName = RuleRef.prettyPrint(key, ~assms=ctx.localFactNames)
      let insts = rule->Rule.genSchemaInsts(gen, ~scope=ctx.fixes)
      let res = rule->Rule.instantiate(insts)
      if !Judgment.concrete(res.conclusion) {
        None
      } else {
        let substs = Judgment.unify(res.conclusion, j, ~gen)->Seq.take(seqSizeLimit)->Seq.toArray
        let makeNew = (subst) => {
          ruleName: key,
          instantiation: insts->Array.map(i => Term.substitute(i, subst)->Term.reduce),
          subgoals: res.premises->Array.map(f),
        }
        
        switch substs {
        | [] => None
        | [subst] => Some(Results.Action(`intro ${keyName}`, makeNew(subst), subst))
        | _ =>
          Some(
            Delay(
              `intro ${keyName}`,
              () =>
                substs->Array.map(subst => {
                  let s =
                    rule.vars
                    ->Belt.Array.reverse
                    ->Belt.Array.zip(insts->Array.map(t => t->Term.substitute(subst)))
                    ->Array.map(
                      ((v, x)) => {
                        let metaS = Term.prettyPrintMeta(v)
                        // not very clean, but don't particularly want to
                        // pollute TERM with another method for printing bare meta
                        let metaWithoutDot =
                          metaS->String.slice(~start=0, ~end=String.length(metaS) - 1)
                        `${metaWithoutDot} ↦ ${Term.prettyPrint(x, ~grammar=Term.emptyGrammar, ~scope=ctx.fixes)}`
                      },
                    )
                    ->Array.join(", ")
                  Results.Action(s, makeNew(subst), subst)
                }),
            ),
          )
        }
      }
    })->Results.atGoal
  }
  let check = (it: t<'a>, ctx: Context.t, j: Judgment.t, f: ('a, Rule.t) => 'b) => {
    let keyName = RuleRef.prettyPrint(it.ruleName, ~assms=ctx.localFactNames);
    switch ctx->Context.lookup(it.ruleName) {
    | None => Error(`Cannot find rule '${keyName}'`)
    | Some(rule) if Array.length(rule.vars) == Array.length(it.instantiation) => {
        let {premises, conclusion} = Rule.instantiate(rule, it.instantiation)
        if Judgment.equivalent(Judgment.reduce(conclusion), Judgment.reduce(j)) {
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
          let concString = Judgment.prettyPrint(conclusion, ~grammar=Term.emptyGrammar, ~scope=ctx.fixes)
          let goalString = Judgment.prettyPrint(j, ~grammar=Term.emptyGrammar, ~scope=ctx.fixes)
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
}

module Elimination = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module Rule = Rule.Make(Term, Judgment)
  module Context = Context(Term, Judgment)
  module Results = MethodResults(Term)
  type t<'a> = {
    ruleName: RuleRef.t,
    elimName: RuleRef.t,
    instantiation: array<Term.t>,
    subgoals: array<'a>,
  }
  exception InternalParseError(string)
  let keywords = ["elim"]
  let prettyPrint = (
    it: t<'a>,
    ~grammar,
    ~scope,
    ~assms,
    ~indentation=0,
    ~subprinter: ('a, ~grammar: Term.grammar, ~scope: array<Term.meta>, ~assms: array<string>, ~indentation: int=?) => string,
  ) => {
    let subgoalsSpacer = if Array.length(it.subgoals) > 0 {
      newline
    } else {
      ""
    }

    let ruleName = RuleRef.prettyPrint(it.ruleName, ~assms)
    let elimName = RuleRef.prettyPrint(it.elimName, ~assms)
    let instantiation = Array.join(
      it.instantiation->Array.map(t => Term.prettyPrint(t, ~grammar, ~scope)),
      " ",
    )    
    let subgoalsStr =
      it.subgoals
      ->Array.map(s => subprinter(s, ~grammar, ~scope, ~assms, ~indentation=indentation + 2))
      ->Array.join(newline)
    `elim (${ruleName} ${elimName} ${instantiation}) {${subgoalsSpacer}${subgoalsStr}}`
  }
  type key = int
  let subproofs = it => it.subgoals -> Array.mapWithIndex((x,i) => (i,x))

  let setSubproof = (it, key, sg) => {
    let newsgs = it.subgoals->Array.copy
    newsgs->Array.set(key, sg)
    {...it, subgoals: newsgs}
  }

  let map = (it: t<'a>, f) => {
    {
      ruleName: it.ruleName,
      elimName: it.elimName,
      instantiation: it.instantiation,
      subgoals: it.subgoals->Array.map(f),
    }
  }

  let substitute = (it: t<'a>, subst: Term.subst) => {
    {
      ruleName: it.ruleName,
      elimName: it.elimName,
      instantiation: it.instantiation->Array.map(t => t->Term.substitute(subst)),
      subgoals: it.subgoals,
    }
  }

  let parse = (input, ~keyword as _, ~grammar, ~scope, ~assms, ~gen, ~subparser) => {
    let cur = ref(String.trim(input))
    if cur.contents->String.get(0) == Some("(") {

      RuleRef.parse(String.trim(cur.contents->String.sliceToEnd(~start=1)),~assms)->Result.flatMap(((
        ruleName,
        rest,
      )) => {
        cur := rest
        RuleRef.parse(String.trim(cur.contents),~assms)->Result.flatMap(((elimName, rest)) => {
          cur := rest
          let instantiation = []
          let it = ref(Error(""))
          while {
            it := Term.parse(cur.contents, ~grammar, ~scope, ~gen)
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
                  switch subparser(cur.contents, ~grammar, ~scope, ~assms, ~gen) {
                  | Ok((sg, rest)) => {
                      Array.push(subgoals, sg)
                      cur := String.trim(rest)
                    }
                  | Error(e) => throw(InternalParseError(e))
                  }
                }
                if cur.contents->String.get(0) == Some("}") {
                  cur := String.trim(cur.contents->String.sliceToEnd(~start=1))
                  let res = {ruleName, elimName, instantiation, subgoals}
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
    let ruleNameS = RuleRef.prettyPrint(it.ruleName, ~assms=ctx.localFactNames);
    let elimNameS = RuleRef.prettyPrint(it.elimName, ~assms=ctx.localFactNames);
    switch (ctx->Context.lookup(it.ruleName), ctx->Context.lookup(it.elimName)) {      
    | (None, _) => Error(`Cannot find rule '${ruleNameS}'`)
    | (_, None) => Error(`Cannot find elimination fact '${elimNameS}'`)
    | (Some(rule), Some(elim)) if rule.premises->Array.length > 0 => {
        let {premises, conclusion} = Rule.instantiate(rule, it.instantiation)
        let elimPremise = premises[0]->Option.getExn
        let remainingPremises = premises->Array.sliceToEnd(~start=1)
        if elimPremise.premises->Array.length > 0 {
          Error(`Premise to eliminate in rule ${ruleNameS} has non-empty premises`)
        } else if elim.premises->Array.length > 0 {
          Error(`Elimination motive (?) ${elimNameS} has non-empty premises`)
        } else if !Judgment.equivalent(Judgment.reduce(elimPremise.conclusion), Judgment.reduce(elim.conclusion)) {
          Error(`Premise to eliminate and elimination motive (?) ${elimNameS} do not match`)
        } else if !Judgment.equivalent(Judgment.reduce(conclusion), Judgment.reduce(j)) {
          let concString = Judgment.prettyPrint(conclusion, ~grammar=Term.emptyGrammar, ~scope=ctx.fixes)
          let goalString = Judgment.prettyPrint(j, ~grammar=Term.emptyGrammar, ~scope=ctx.fixes)
          Error(`Conclusion of rule '${concString}' doesn't match goal '${goalString}'`)
        } else if Array.length(it.subgoals) != Array.length(remainingPremises) {
          let subgoalsRem = Array.length(it.subgoals)->Int.toString
          let premsRem = Array.length(remainingPremises)->Int.toString
          Error(
            `Number of subgoals (${subgoalsRem}) doesn't match rule ${ruleNameS}'s remaining number (${premsRem})`,
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
    | (Some(_), Some(_)) => Error(`Rule ${ruleNameS} doesn't have any premises`)
    }
  }

  let apply = (ctx: Context.t, j: Judgment.t, gen: Term.gen, f: Rule.t => 'a): Results.attached<t<'a>> => {
    let possibleRules =
      ctx.globalFacts
      ->Dict.toArray
      ->Array.filter(((_, r)) =>
        r.premises->Array.length > 0 && {
            let fst = r.premises[0]->Option.getExn
            fst.premises->Array.length == 0 && fst.vars->Array.length == 0
          }
      )
    let possibleElims =
      ctx.localFacts
      ->Array.filter(r => r.premises->Array.length == 0 && r.vars->Array.length == 0)

    let byAssumption =
      possibleElims->Array.mapWithIndex((elim, i) => {
        let elimName = RuleRef.Local({index: i})
        let elimNameS = ctx.localFactNames[i]->Option.getExn
        Results.atAssumption(
          i,
          [
            Results.Delay(
              "elim " ++ elimNameS,
              () => {
                let subtree = []
                possibleRules->Array.forEach(((ruleNameS, rule)) => {
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
                          let subst = Term.mergeSubsts(elimSub, ruleSub)
                          let values = ruleInsts->Array.map(i => Term.substitute(i, subst)->Term.reduce)
                          let new = {
                            ruleName: RuleRef.Global({name: ruleNameS}),
                            elimName,
                            instantiation: values,
                            subgoals: rule.premises->Array.sliceToEnd(~start=1)->Array.map(f),
                          }
                          subtree->Array.push(Results.Action("with " ++ ruleNameS, new, subst))
                        },
                      )
                    },
                  )
                })
                subtree
              },
            ),
          ],
        )
      })

    byAssumption->Array.reduce(Results.emptyAttached(), Results.combine)
  }
}

module Lemma = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module Rule = Rule.Make(Term, Judgment)
  module Context = Context(Term, Judgment)
  module Results = MethodResults(Term)
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
  let substitute = (it: t<'a>, subst: Term.subst) => {
    {
      rule: it.rule->Rule.substitute(subst),
      proof: it.proof,
      show: it.show,
    }
  }
  let keywords = ["have"]
  let prettyPrint = (
    it: t<'a>,
    ~grammar,
    ~scope,
    ~assms,
    ~indentation=0,
    ~subprinter: ('a, ~grammar: Term.grammar, ~scope: array<Term.meta>, ~assms: array<string>, ~indentation: int=?) => string,
  ) => {
    "have "
    ->String.concat(Rule.prettyPrintInline(it.rule, ~grammar, ~scope))
    ->String.concat(newline)
    ->String.concat(subprinter(it.proof, ~grammar, ~scope, ~assms,~indentation))
    ->String.concat(newline)
    ->String.concat(subprinter(it.show, ~grammar, ~scope, ~assms, ~indentation))
  }
  let parse = (input, ~keyword as _, ~grammar, ~scope, ~assms, ~gen, ~subparser) => {
    //todo add toplevel
    switch Rule.parseInner(input, ~grammar, ~scope, ~gen) {
    | Ok((rule, rest)) =>
      switch subparser(rest, ~grammar, ~scope, ~assms, ~gen) {
      | Ok((proof, rest')) =>
        switch String.trim(rest')->subparser(~grammar, ~scope, ~assms, ~gen) {
        | Ok((show, rest'')) => Ok({rule, proof, show}, rest'')
        | Error(e) => Error(e)
        }
      | Error(e) => Error(e)
      }
    | Error(e) => Error(e)
    }
  }
  let apply = (_ctx: Context.t, _j: Judgment.t, _gen: Term.gen, _f: Rule.t => 'a) => {
    Results.emptyAttached()
  }
  type key = Proof | Show
  let subproofs = it => [(Proof,it.proof),(Show,it.show)]
  
  let setSubproof = (it, key, sg) => switch key {
  | Proof => {...it, proof: sg}
  | Show  => {...it, show:  sg}
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
  module Results = MethodResults(Term)
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
  type key = FirstK(Method1.key) | SecondK(Method2.key)
  let subproofs = it => 
    switch it {
    | First(m) => Method1.subproofs(m)->Array.map( ((k,v)) => (FirstK(k),v) )
    | Second(m) => Method2.subproofs(m)->Array.map( ((k,v)) => (SecondK(k),v) )
    }

  let setSubproof = (it, key, sg) => {
    switch (it,key) {
    | (First(m), FirstK(k)) => First(Method1.setSubproof(m,k,sg))
    | (Second(m), SecondK(k)) => Second(Method2.setSubproof(m,k,sg))
    | _ => it // impossible
    }
  }
  
  let apply = (ctx: Context.t, j: Judgment.t, gen: Term.gen, f: Rule.t => 'a) => {
    Results.combine(Method1.apply(ctx, j, gen, f)->Results.mapAttached(m => First(m)),
      Method2.apply(ctx, j, gen, f)->Results.mapAttached(m => Second(m)))
  }
  let check = (it, ctx, j, f) =>
    switch it {
    | First(m) => m->Method1.check(ctx, j, f)->Result.map(x => First(x))
    | Second(m) => m->Method2.check(ctx, j, f)->Result.map(x => Second(x))
    }
  let prettyPrint = (it: t<'a>, ~grammar, ~scope, ~assms, ~indentation=0, ~subprinter) =>
    switch it {
    | First(m) => m->Method1.prettyPrint(~grammar, ~scope, ~assms, ~indentation, ~subprinter)
    | Second(m) => m->Method2.prettyPrint(~grammar, ~scope, ~assms, ~indentation, ~subprinter)
    }
  let parse = (input, ~keyword, ~grammar, ~scope, ~assms, ~gen, ~subparser) => {
    if Method1.keywords->Array.indexOf(keyword) > -1 {
      Method1.parse(input, ~keyword, ~grammar, ~scope, ~assms, ~gen, ~subparser)->Result.map(((x, r)) => (
        First(x),
        r,
      ))
    } else {
      Method2.parse(input, ~keyword, ~grammar, ~scope, ~assms, ~gen, ~subparser)->Result.map(((x, r)) => (
        Second(x),
        r,
      ))
    }
  }
}
