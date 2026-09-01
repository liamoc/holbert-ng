open Signatures
module Make = (Term: TERM, Judgment : REWRITABLE_JUDGMENT with module Term := Term) => {
  module Rule = Rule.Make(Term, Judgment)
  module Context = Method.Context(Term, Judgment)
  module Results = Method.MethodResults(Term)

  type direction = Forward | Backward
  type t<'a> = {
    ruleName: Method.RuleRef.t,
    direction: direction,
    path: Judgment.path,
    values: array<Term.t>,
    newGoal: 'a,
    subgoals: array<'a>,
  }
  
  let keywords = ["rewrite","rev_rewrite"]
  type key = NewGoal | Subgoal(int)
  let subproofs = (step: t<'a>): array<(key, 'a)> =>
    Array.concat([(NewGoal, step.newGoal)], step.subgoals->Array.mapWithIndex((v, i) => (Subgoal(i), v)))
  let setSubproof = (step: t<'a>, k: key, newVal: 'a): t<'a> =>
    switch k {
    | NewGoal => {...step, newGoal: newVal}
    | Subgoal(i) => {...step, subgoals: Util.updateAtIndex(step.subgoals, i, newVal)}
    }
  let substitute = (step: t<'a>, subst: Term.subst): t<'a> => {
    ...step,
    values: step.values->Array.map(v => Term.substitute(v, subst)),
  }

  let map = (step: t<'a>, f: 'a => 'b): t<'b> => {
    ruleName: step.ruleName,
    direction: step.direction,
    path: step.path,
    values: step.values,
    newGoal: f(step.newGoal),
    subgoals: step.subgoals->Array.map(f),
  }
  let orient = (dir, lhs, rhs) =>
    switch dir {
    | Forward => (lhs, rhs)
    | Backward => (rhs, lhs)
    }
  let check = (step: t<'a>, context: Context.t, goal: Judgment.t, checkSubgoal): result<t<'b>, string> => {
    let keyName = Method.RuleRef.prettyPrint(step.ruleName, ~assms=context.localFactNames)
    switch context->Context.lookup(step.ruleName) {
    | None => Error(`no such rule: ${keyName}`)
    | Some(rule) =>
      if Array.length(step.values) != Array.length(rule.Rule.vars) {
        Error(`wrong number of instantiations for rule ${keyName}`)
      } else {
        switch Judgment.locate(goal, step.path) {
        | None => Error("invalid rewrite path")
        | Some((subterm, localScope)) =>
          let {premises, conclusion} = rule->Rule.instantiate(step.values)
          switch Judgment.asEquation(conclusion) {
          | None => Error(`rule ${keyName} is not an equation`)
          | Some((eqLhs, eqRhs)) =>
            let (lhs, rhs) = orient(step.direction, eqLhs, eqRhs)
            let lhs' = Term.upshift(lhs, localScope->Array.length)
            if !Term.equivalent(lhs', subterm) {
              Error(`instantiated left-hand side (${lhs'->Term.prettyPrint(~grammar=Term.emptyGrammar,~scope=context.fixes)}) 
                     does not match the term at the given path 
                     (${subterm->Term.prettyPrint(~grammar=Term.emptyGrammar, ~scope=context.fixes)})`)
            } else {
              let newGoal = Judgment.replaceAt(goal, step.path, Term.reduce(Term.upshift(rhs, localScope->Array.length)))
              if Array.length(premises) != Array.length(step.subgoals) {
                Error("subgoal count mismatch")
              } else {
                Ok({
                  ruleName: step.ruleName,
                  direction: step.direction,
                  path: step.path,
                  values: step.values,
                  newGoal: checkSubgoal(step.newGoal, {Rule.vars: [], premises: [], conclusion: newGoal}),
                  subgoals: step.subgoals->Array.mapWithIndex((a, i) =>
                    checkSubgoal(a, premises->Array.getUnsafe(i))
                  ),
                })
              }
            }
          }
        }
      }
    }  
  }
  let apply = (
    context: Context.t,
    goal: Judgment.t,
    gen: Term.gen,
    mkSubgoal: Rule.t => 'a,
  ): Results.attached<t<'a>> => {
    let actionsFor = (ruleName, rule: Rule.t): array<(t<'a>, Term.subst)> =>
      switch Judgment.asEquation(rule.conclusion) {
      | None => []
      | Some(_) => {
          let insts = rule->Rule.genSchemaInsts(gen, ~scope=context.fixes)
          let {premises, conclusion} = rule->Rule.instantiate(insts)
          switch Judgment.asEquation(conclusion) {
          | None => []
          | Some((eqLhs, eqRhs)) =>
            [Forward, Backward]->Array.flatMap(direction => {
              let (lhs, rhs) = orient(direction, eqLhs, eqRhs)
              if !Term.concrete(lhs) {
                []
              } else {
                Judgment.positions(goal)->Array.filterMap(((path, subterm, boundScope)) =>
                  switch Term.unify(Term.upshift(lhs, boundScope->Array.length), subterm, ~gen)->Seq.head {
                  | None => None
                  | Some(subst) =>
                    let values = insts->Array.map(i => Term.substitute(i, subst)->Term.reduce)
                    let replacement = Term.reduce(Term.substitute(Term.upshift(rhs, boundScope->Array.length), subst))
                    let newGoal = Judgment.replaceAt(goal, path, replacement)
                    let newGoalRule: Rule.t = {Rule.vars: [], premises: [], conclusion: newGoal}
                    let subgoalRules = premises->Array.map(p => p->Rule.substitute(subst))
                    Some((
                      {
                        ruleName,
                        direction,
                        path,
                        values,
                        newGoal: newGoalRule->mkSubgoal,
                        subgoals: subgoalRules->Array.map(mkSubgoal),
                      },
                      subst,
                    ))
                  }
                )
              }
            })
          }
        }
      }
    
    let dirLabel = d => switch d { | Forward => "" | Backward => " ⤺" }

    context
    ->Context.facts
    ->Array.map(((ruleName, rule)) => {
      let keyName = Method.RuleRef.prettyPrint(ruleName, ~assms=context.localFactNames)
      let actions = switch actionsFor(ruleName, rule) {
      | [] => []
      | [(step, subst)] => [Results.Action(`rewrite ${keyName} ${dirLabel(step.direction)}`, step, subst)]
      | many => [
          Results.Group(
            `rewrite ${keyName}`,
            many->Array.map(((step, subst)) => 
              Results.Action(`${Judgment.prettyPrintPath(step.path)} ${dirLabel(step.direction)}`, step, subst)),
          ),
        ]
      }
      {Results.goal: actions, assumptions: []}
    })->Array.reduce(Results.emptyAttached(), Results.combine)
  }

  let prettyPrint = (
      it: t<'a>,
      ~grammar,
      ~scope,
      ~assms,
      ~indentation=0,
      ~subprinter: ('a, ~grammar: Term.grammar, ~scope: array<Term.meta>, ~assms: array<string>, ~indentation: int=?) => string,
    ) => {
      let args = it.values->Array.map(t => Term.prettyPrint(t, ~grammar, ~scope))
      (if it.direction == Backward { "rev_rewrite" } else { "rewrite" })
      ->String.concat(" (")
      ->String.concat(Array.join([Method.RuleRef.prettyPrint(it.ruleName,~assms)]->Array.concat(args), " "))
      ->String.concat(") ")
      ->String.concat(Judgment.prettyPrintPath(it.path))
      ->String.concat(" {")
      ->String.concat(
        if Array.length(it.subgoals) > 0 {
          Util.newline
        } else {
          ""
        },
      )
      ->String.concat(
        it.subgoals
        ->Array.map(s => subprinter(s, ~grammar, ~scope, ~assms, ~indentation=indentation + 2))
        ->Array.join(Util.newline),
      )
      ->String.concat("}")
      ->String.concat(Util.newline)
      ->String.concat(subprinter(it.newGoal, ~grammar, ~scope, ~assms, ~indentation))
      ->String.concat(Util.newline)
    }

  exception InternalParseError(string)

  let parse = (input, ~keyword, ~grammar, ~scope, ~assms, ~gen, ~subparser) => {
    let direction = if keyword == "rev_rewrite" { Backward } else { Forward }  
    let cur = ref(String.trim(input))
    if cur.contents->String.get(0) == Some("(") {
      switch Method.RuleRef.parse(String.trim(cur.contents->String.sliceToEnd(~start=1)),~assms) {
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
            let (path,rest) = Judgment.parsePath(cur.contents);
            cur := String.trim(rest);
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
                  switch subparser(cur.contents, ~grammar, ~scope, ~assms, ~gen) {
                  | Ok ((newGoal, rest)) => {
                      cur := String.trim(rest)
                      Ok(({ruleName, direction, values:instantiation, path, newGoal, subgoals}, cur.contents))
                    }
                  | Error(e) => throw(InternalParseError(e))
                  }
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


  let updateAtKey = (it: t<'a>, key: int, f: 'a => 'a) => {
    let newsgs = it.subgoals->Array.copy
    newsgs->Array.set(key, f(newsgs[key]->Option.getExn))
    {...it, subgoals: newsgs}
  }

  let updateGoal = (it: t<'a>, f: 'a => 'a) => {
    {...it, newGoal: f(it.newGoal)}
  }

}