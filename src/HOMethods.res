module ConstructorDisjointness = {
  module Term = HOTerm
  module Judgment = HOTerm
  module Rule = Rule.Make(Term, Judgment)
  module Context = Method.Context(Term, Judgment)
  module Results = Method.MethodResults(Term)

  type t<'a> = {factName: Method.RuleRef.t}

  let keywords = ["disjointness"]

  let substitute = (step: t<'a>, _subst: Term.subst): t<'a> => step
  let map = (step: t<'a>, _f: 'a => 'b): t<'b> => {factName: step.factName}
  type key = unit
  let subproofs = _ => []
  let setSubproof = (it,_,_) => it
  let ctorHead = t =>
    switch Term.strip(t) {
    | (Term.Symbol({name, constructor: true}), args) => Some((name, Array.length(args)))
    | _ => None
    }

  let isDisjointEquation = eqn =>
    switch Term.asEquation(eqn) {
    | None => false
    | Some((a, b)) =>
      switch (ctorHead(a), ctorHead(b)) {
      | (Some((n1, k1)), Some((n2, k2))) => n1 != n2 || k1 != k2
      | _ => false
      }
    }

  let check = (
    step: t<'a>,
    context: Context.t,
    _goal: Judgment.t,
    _checkSubgoal: ('a, Rule.t) => 'b,
  ): result<t<'b>, string> => {
    let keyName = Method.RuleRef.prettyPrint(step.factName, ~assms=context.localFactNames)
    switch context->Context.lookup(step.factName) {
    | None => Error(`no such fact: ${keyName}`)
    | Some({Rule.vars: [], premises: [], conclusion}) =>
      isDisjointEquation(conclusion) 
        ? Ok({factName: step.factName}) 
        : Error(`fact ${keyName} is not a constructor disjointness`)
    | Some(_) => Error(`fact ${keyName} is not a plain assumption`)
    }
  }

  let apply = (
    context: Context.t,
    _goal: Judgment.t,
    _gen: Term.gen,
    _mkSubgoal: Rule.t => 'a,
  ): Results.attached<t<'a>> =>
    context.localFacts
    ->Array.mapWithIndex((rule, i) => (i, rule, context.localFactNames->Belt.Array.get(i)))
    ->Array.filterMap(((i, rule, nameOpt)) =>
      switch (rule, nameOpt) {
      | ({Rule.vars: [], premises: []}, Some(factName)) if isDisjointEquation(rule.conclusion) =>
        Some(
          Results.atAssumption(
            i,
            [Results.Action(`disjointness ${factName}`, {factName: Local({index: i})}, Term.makeSubst())],
          ),
        )
      | _ => None
      }
    )->Array.reduce(Results.emptyAttached(), Results.combine)
    
  let prettyPrint = (
      it: t<'a>,
      ~grammar as _,
      ~scope as _,
      ~assms,
      ~indentation as _=0,
      ~subprinter as _: ('a, ~grammar: Term.grammar, ~scope: array<Term.meta>, ~assms: array<string>, ~indentation: int=?) => string,
    ) => 
      "disjointness "
      ->String.concat(Method.RuleRef.prettyPrint(it.factName, ~assms))
      ->String.concat(Util.newline)

  let parse = (input, ~keyword as _, ~grammar as _, ~scope as _, ~assms, ~gen as _, ~subparser as _) =>
    switch Method.RuleRef.parse(String.trim(input),~assms) {
    | Ok((ruleName, rest)) => Ok(({factName: ruleName},rest))
    | _ => Error("Expected fact name")
    }
}


module ConstructorInjectivity = {
  module Term = HOTerm
  module Judgment = HOTerm
  module Rule = Rule.Make(Term, Judgment)
  module Context = Method.Context(Term, Judgment)
  module Results = Method.MethodResults(Term)

  type t<'a> = {factName: Method.RuleRef.t, subgoal: 'a}

  let keywords = ["injectivity"]

  let substitute = (step: t<'a>, _subst: Term.subst): t<'a> => step
  let map = (step: t<'a>, f: 'a => 'b): t<'b> => {factName: step.factName, subgoal: f(step.subgoal)}
  type key = unit
  
  let subproofs = it => [((),it.subgoal)]

  let setSubproof = (it: t<'a>, _key: unit, g) => {
    {...it, subgoal: g}
  }
  // (= (@A a1..an) (@A b1..bn)) -> Some([(a1,b1), ..., (an,bn)])
  let injectivityPairs = (eqn: Term.t): option<array<(Term.t, Term.t)>> =>
    switch Term.asEquation(eqn) {
    | None => None
    | Some((a, b)) =>
      switch (Term.strip(a), Term.strip(b)) {
      | ((Term.Symbol({name: n1, constructor: true}), argsA), (Term.Symbol({name: n2, constructor: true}), argsB))
        if n1 == n2 && Array.length(argsA) == Array.length(argsB) =>
        Some(Belt.Array.zip(argsA, argsB))
      | _ => None
      }
    }

  let mkPremiseRule = ((x, y)): Rule.t => {Rule.vars: [], premises: [], conclusion: Term.mkEquation(x, y)}

  let check = (
    step: t<'a>,
    context: Context.t,
    goal: Judgment.t,
    checkSubgoal: ('a, Rule.t) => 'b,
  ): result<t<'b>, string> => {
    let keyName = Method.RuleRef.prettyPrint(step.factName, ~assms=context.localFactNames)
    switch context->Context.lookup(step.factName) {
    | None => Error(`no such fact: ${keyName}`)
    | Some({Rule.vars: [], premises: []} as rule) =>
      switch injectivityPairs(rule.conclusion) {
      | None => Error(`fact ${keyName} is not a constructor equality`)
      | Some(pairs) =>
        Ok({
          factName: step.factName,
          subgoal: checkSubgoal(
            step.subgoal,
            {Rule.vars: [], premises: pairs->Array.map(mkPremiseRule), conclusion: goal},
          ),
        })
      }
    | Some(_) => Error(`fact ${keyName} is not a plain assumption`)
    }
  }

  let apply = (
    context: Context.t,
    goal: Judgment.t,
    _gen: Term.gen,
    mkSubgoal: Rule.t => 'a,
  ): Results.attached<t<'a>> =>
      context.localFacts
      ->Array.mapWithIndex((rule, i) => (i, rule, context.localFactNames->Belt.Array.get(i)))
      ->Array.filterMap(((i, rule, nameOpt)) =>
        switch (rule, nameOpt) {
        | ({Rule.vars: [], premises: []}, Some(factName)) =>
          switch injectivityPairs(rule.conclusion) {
          | None => None
          | Some(pairs) =>
            let subgoalRule: Rule.t = {
              Rule.vars: [],
              premises: pairs->Array.map(mkPremiseRule),
              conclusion: rule.conclusion,
            }
            Some(
              Results.atAssumption(
                i,
                [
                  Results.Action(
                    `injectivity ${factName}`,
                    {factName:Local({index:i}), subgoal: subgoalRule->mkSubgoal},
                    Term.makeSubst(),
                  ),
                ],
              ),
            )
          }
        | _ => None
        }
      )->Array.reduce(Results.emptyAttached(), Results.combine)
   
  let prettyPrint = (
        it: t<'a>,
        ~grammar,
        ~scope,
        ~assms,
        ~indentation=0,
        ~subprinter: ('a, ~grammar: Term.grammar, ~scope: array<Term.meta>, ~assms: array<string>, ~indentation: int=?) => string,
      ) =>
      "injectivity "
      ->String.concat(Method.RuleRef.prettyPrint(it.factName, ~assms))
      ->String.concat(Util.newline)
      ->String.concat(subprinter(it.subgoal, ~grammar, ~scope, ~assms, ~indentation))
      ->String.concat(Util.newline)
  exception InternalParseError(string)
    
  let parse = (input, ~keyword as _, ~grammar, ~scope, ~assms, ~gen, ~subparser) => {
    switch Method.RuleRef.parse(String.trim(input),~assms) {
    | Ok((ruleName, rest)) => {
        switch subparser(String.trim(rest), ~grammar, ~scope, ~assms, ~gen) {
        | Ok((sg, rest)) =>
            Ok(({subgoal:sg, factName:ruleName}, rest))
        | Error(e) => Error(e)
        }
      }
    | _ => Error("Expected fact name")
    }
  }

        
}