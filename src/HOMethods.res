module ConstructorDisjointness = {
  module Term = HOTerm
  module Judgment = HOTerm
  module Rule = Rule.Make(Term, Judgment)
  module Context = Method.Context(Term, Judgment)
  module Results = Method.MethodResults(Term)

  type t<'a> = {factName: string}

  let keywords = ["disjointness"]

  let substitute = (step: t<'a>, _subst: Term.subst): t<'a> => step
  let map = (step: t<'a>, _f: 'a => 'b): t<'b> => {factName: step.factName}

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
  ): result<t<'b>, string> =>
    switch context->Context.facts->Dict.get(step.factName) {
    | None => Error(`no such fact: ${step.factName}`)
    | Some({Rule.vars: [], premises: [], conclusion}) =>
      isDisjointEquation(conclusion) 
        ? Ok({factName: step.factName}) 
        : Error(`fact ${step.factName} is not a constructor disjointness`)
    | Some(_) => Error(`fact ${step.factName} is not a plain assumption`)
    }

  let apply = (
    context: Context.t,
    _goal: Judgment.t,
    _gen: Term.gen,
    _mkSubgoal: Rule.t => 'a,
  ): array<Results.t<t<'a>>> =>
    context
    ->Context.facts
    ->Dict.toArray
    ->Array.filterMap(((factName, rule)) =>
      switch rule {
      | {Rule.vars: [], premises: [], conclusion} if isDisjointEquation(conclusion) =>
        Some(Results.Action(`disjointness ${factName}`, {factName: factName}, Term.makeSubst()))
      | _ => None
      }
    )
  let prettyPrint = (
      it: t<'a>,
      ~scope,
      ~indentation=0,
      ~subprinter: ('a, ~scope: array<Term.meta>, ~indentation: int=?) => string,
    ) =>
      "disjointness "
      ->String.concat(it.factName)
      ->String.concat(Util.newline)

  let parse = (input, ~keyword, ~scope, ~gen, ~subparser) =>
    switch Rule.parseRuleName(String.trim(input)) {
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

  type t<'a> = {factName: string, subgoal: 'a}

  let keywords = ["injectivity"]

  let substitute = (step: t<'a>, _subst: Term.subst): t<'a> => step
  let map = (step: t<'a>, f: 'a => 'b): t<'b> => {factName: step.factName, subgoal: f(step.subgoal)}

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
  ): result<t<'b>, string> =>
    switch context->Context.facts->Dict.get(step.factName) {
    | None => Error(`no such fact: ${step.factName}`)
    | Some({Rule.vars: [], premises: []} as rule) =>
      switch injectivityPairs(rule.conclusion) {
      | None => Error(`fact ${step.factName} is not a constructor equality`)
      | Some(pairs) =>
        Ok({
          factName: step.factName,
          subgoal: checkSubgoal(
            step.subgoal,
            {Rule.vars: [], premises: pairs->Array.map(mkPremiseRule), conclusion: goal},
          ),
        })
      }
    | Some(_) => Error(`fact ${step.factName} is not a plain assumption`)
    }

  let apply = (
    context: Context.t,
    goal: Judgment.t,
    _gen: Term.gen,
    mkSubgoal: Rule.t => 'a,
  ): array<Results.t<t<'a>>> =>
    context
    ->Context.facts
    ->Dict.toArray
    ->Array.filterMap(((factName, rule)) =>
      switch rule {
      | {Rule.vars: [], premises: []} =>
        switch injectivityPairs(rule.conclusion) {
        | None => None
        | Some(pairs) =>
          let subgoalRule: Rule.t = {
            Rule.vars: [],
            premises: pairs->Array.map(mkPremiseRule),
            conclusion: goal,
          }
          Some(
            Results.Action(
              `injectivity ${factName}`,
              {factName, subgoal: subgoalRule->mkSubgoal},
              Term.makeSubst(),
            ),
          )
        }
      | _ => None
      }
    )
   
  let prettyPrint = (
        it: t<'a>,
        ~scope,
        ~indentation=0,
        ~subprinter: ('a, ~scope: array<Term.meta>, ~indentation: int=?) => string,
      ) =>
      "injectivity "
      ->String.concat(it.factName)
      ->String.concat(Util.newline)
      ->String.concat(subprinter(it.subgoal, ~scope, ~indentation))
      ->String.concat(Util.newline)
  exception InternalParseError(string)
    
  let parse = (input, ~keyword, ~scope, ~gen, ~subparser) => {
    switch Rule.parseRuleName(String.trim(input)) {
    | Ok((ruleName, rest)) => {
        switch subparser(String.trim(rest), ~scope, ~gen) {
        | Ok((sg, rest)) =>
            Ok(({subgoal:sg, factName:ruleName}, rest))
        | Error(e) => Error(e)
        }
      }
    | _ => Error("Expected fact name")
    }
  }

  let updateGoal = (it: t<'a>, f: 'a => 'a) => {
    {...it, subgoal: f(it.subgoal)}
  }
        
}