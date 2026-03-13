open Signatures
open Component

// InductiveSet is specific to HOTerm to allow pattern matching on term structure
module Make = (
  Term: TERM with type t = HOTerm.t and type meta = string,
  Judgment: JUDGMENT with module Term := Term and type t = HOTerm.t,
  JudgmentView: JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment,
) => {
  module Rule = Rule.Make(Term, Judgment)
  module RuleView = RuleView.Make(Term, Judgment, JudgmentView)
  module Ports = Ports(Term, Judgment)
  type state = dict<Rule.t>
  type props = {
    content: state,
    imports: Ports.t,
    onChange: (state, ~exports: Ports.t=?) => unit,
  }

  type predicateGroup = {
    name: string,
    arity: int,
    rules: array<(string, Rule.t)>,
  }

  let makeKey = (name, arity) => name ++ "§" ++ Int.toString(arity)
  let makeVarNames = (arity: int) =>
    Array.fromInitializer(~length=arity, i => "§" ++ Int.toString(i))
  let makeVarArgs = (arity: int) => Array.fromInitializer(~length=arity, i => HOTerm.Var({idx: i}))

  let extractPredicateSignature = (rule: Rule.t): option<(string, int)> => {
    let (head, args) = HOTerm.strip(rule.conclusion)
    switch head {
    | Symbol({name}) => Some((name, Array.length(args)))
    | _ => None
    }
  }

  let groupByPredicate = (rules: dict<Rule.t>): array<predicateGroup> =>
    rules
    ->Dict.toArray
    ->Array.filterMap(((name, rule)) =>
      extractPredicateSignature(rule)->Option.map(sig => (name, rule, sig))
    )
    ->Array.reduce(Dict.make(), (acc, (name, rule, (cname, arity))) => {
      let key = makeKey(cname, arity)
      Dict.set(acc, key, [(name, rule), ...Dict.get(acc, key)->Option.getOr([])])
      acc
    })
    ->Dict.valuesToArray
    ->Array.map(predicates => {
      let (_, firstRule) = predicates[0]->Option.getExn
      let (name, arity) = extractPredicateSignature(firstRule)->Option.getExn
      {name, arity, rules: predicates}
    })
  let generateInductionRule = (group: predicateGroup, allGroups: array<predicateGroup>): Rule.t => {
    let {name: str, arity: i} = group
    let numFormers = Array.length(allGroups)
    let groupIndex = Util.mustFindIndex(allGroups, g => g.name == str && g.arity == i)

    let findFormerIndex = (name, arity) =>
      Util.mustFindIndex(allGroups, g => g.name == name && g.arity == arity)

    let generateInductiveHypothesis = (premise: Rule.t, offset: int): option<Rule.t> => {
      let (head, args) = HOTerm.strip(premise.conclusion)
      switch head {
      | Symbol({name}) =>
        let formerIndex = findFormerIndex(name, Array.length(args))
        Some({
          Rule.vars: premise.vars,
          premises: premise.premises,
          conclusion: HOTerm.app(
            HOTerm.Var({idx: offset + Array.length(premise.vars) + i + formerIndex}),
            args,
          ),
        })
      | _ => None
      }
    }

    let caseSubgoal = (constructorRule: Rule.t): Rule.t => {
      let offset = Array.length(constructorRule.vars)
      let inductiveHypotheses =
        constructorRule.premises->Array.filterMap(premise =>
          generateInductiveHypothesis(premise, offset)
        )

      let (conclusionHead, conclusionArgs) = HOTerm.strip(constructorRule.conclusion)
      let typeIndex = switch conclusionHead {
      | Symbol({name}) => findFormerIndex(name, Array.length(conclusionArgs))
      | _ => throw(Util.Unreachable("Constructor conclusion must have a Symbol head"))
      }

      {
        Rule.vars: constructorRule.vars,
        premises: Array.concat(constructorRule.premises, inductiveHypotheses),
        conclusion: HOTerm.app(HOTerm.Var({idx: offset + i + typeIndex}), conclusionArgs),
      }
    }

    let allConstructors = Array.flatMap(allGroups, g => g.rules)
    let subgoals = Array.map(allConstructors, ((_, rule)) => caseSubgoal(rule))

    {
      Rule.vars: Array.concat(
        makeVarNames(i),
        Array.fromInitializer(~length=numFormers, i => "§P" ++ Int.toString(i)),
      ),
      premises: [
        {
          Rule.vars: [],
          premises: [],
          conclusion: HOTerm.app(HOTerm.Symbol({name: str, constructor: false}), makeVarArgs(i)),
        },
        ...subgoals,
      ],
      conclusion: HOTerm.app(HOTerm.Var({idx: i + groupIndex}), makeVarArgs(i)),
    }
  }

  module StringCmp = Belt.Id.MakeComparable({
    type t = string
    let cmp = Pervasives.compare
  })

  let extractInductiveType = (premise: Rule.t): option<(string, int)> => {
    let (head, args) = HOTerm.strip(premise.conclusion)
    switch head {
    | Symbol({name}) => Some((name, Array.length(args)))
    | _ => None
    }
  }

  let isSelfReference = (group: predicateGroup, (name, arity)): bool =>
    name == group.name && arity == group.arity

  let findDependencies = (group: predicateGroup): array<(string, int)> =>
    group.rules
    ->Array.flatMap(((_name, rule)) => rule.premises->Array.filterMap(extractInductiveType))
    ->Array.filter(dep => !isSelfReference(group, dep))

  let rec collectReachable = (
    toVisit: array<(string, int)>,
    visited: Belt.Set.t<string, StringCmp.identity>,
    allGroups: array<predicateGroup>,
  ): Belt.Set.t<string, StringCmp.identity> =>
    switch toVisit {
    | [] => visited
    | _ =>
      let (name, arity) = toVisit[0]->Option.getExn
      let rest = Array.sliceToEnd(toVisit, ~start=1)
      let key = makeKey(name, arity)

      if Belt.Set.has(visited, key) {
        collectReachable(rest, visited, allGroups)
      } else {
        let visited = Belt.Set.add(visited, key)
        let newDeps =
          allGroups
          ->Array.find(g => g.name == name && g.arity == arity)
          ->Option.map(findDependencies)
          ->Option.getOr([])
        collectReachable(Array.concat(rest, newDeps), visited, allGroups)
      }
    }

  let findMutuallyInductiveComponent = (
    targetGroup: predicateGroup,
    allGroups: array<predicateGroup>,
  ): array<predicateGroup> => {
    let reachableKeys = collectReachable(
      [(targetGroup.name, targetGroup.arity)],
      Belt.Set.make(~id=module(StringCmp)),
      allGroups,
    )
    allGroups->Array.filter(g => Belt.Set.has(reachableKeys, makeKey(g.name, g.arity)))
  }

  let generateCasesRule = (group: predicateGroup): Rule.t => {
    let {name: str, arity} = group

    let caseSubgoal = ((_constructorName: string, predicateRule: Rule.t)): Rule.t => {
      let offset = Array.length(predicateRule.vars)

      // Extract the argument from the predicate conclusion
      // e.g., from (Nat 0) extract 0, from (Nat (S n)) extract (S n)
      let (_head, args) = HOTerm.strip(predicateRule.conclusion)
      assert(Array.length(args) == arity)

      let equalityPremises = args->Array.mapWithIndex((arg, idx) => {
        {
          Rule.vars: [],
          premises: [],
          conclusion: HOTerm.app(
            HOTerm.Symbol({name: "=", constructor: false}),
            [HOTerm.Var({idx: offset + idx}), arg],
          ),
        }
      })

      {
        Rule.vars: predicateRule.vars,
        premises: Array.concat(equalityPremises, predicateRule.premises),
        conclusion: HOTerm.Var({idx: offset + arity}),
      }
    }

    let subgoals = Array.map(group.rules, ((name, rule)) => caseSubgoal((name, rule)))

    {
      Rule.vars: Array.concat(makeVarNames(arity), ["§P"]),
      premises: [
        {
          Rule.vars: [],
          premises: [],
          conclusion: HOTerm.app(
            HOTerm.Symbol({name: str, constructor: false}),
            makeVarArgs(arity),
          ),
        },
        ...subgoals,
      ],
      conclusion: HOTerm.Var({idx: arity}),
    }
  }

  let derived = (state: state): state =>
    state
    ->groupByPredicate
    ->Array.flatMap(group => {
      let mutualComponent = findMutuallyInductiveComponent(group, groupByPredicate(state))
      let inductionRule = generateInductionRule(group, mutualComponent)
      let casesRule = generateCasesRule(group)
      [("§induction-" ++ group.name, inductionRule), ("§cases-" ++ group.name, casesRule)]
    })
    ->Dict.fromArray
  let serialise = (state: state) =>
    state
    ->Dict.toArray
    ->Array.map(((k, r)) => r->Rule.prettyPrintTopLevel(~name=k))
    ->Array.join("\n")
  let deserialise = (str: string, ~imports as _: Ports.t) => {
    let cur = ref(str)
    let go = ref(true)
    let results = Dict.make()
    let ret = ref(Error("impossible"))
    while go.contents {
      switch Rule.parseTopLevel(cur.contents, ~scope=[]) {
      | Ok((t, n), rest) =>
        if n->String.trim == "" {
          go := false
          ret := Error("Rule given with no name")
        } else {
          Dict.set(results, n, t)
          if rest->String.trim == "" {
            go := false
            ret := Ok(results)
          } else {
            cur := rest
          }
        }
      | Error(e) => {
          go := false
          ret := Error(e)
        }
      }
    }
    ret.contents->Result.map(state => (
      state,
      {Ports.facts: state->Dict.copy->Dict.assign(derived(state)), ruleStyle: None},
    ))
  }

  let make = props => {
    <div
      className={"axiom-set axiom-set-"->String.concat(
        String.make(props.imports.ruleStyle->Option.getOr(Hybrid)),
      )}
    >
      {Dict.toArray(props.content->Dict.copy->Dict.assign(derived(props.content)))
      ->Array.mapWithIndex(((n, r), i) =>
        <RuleView
          rule={r}
          scope={[]}
          key={String.make(i)}
          style={props.imports.ruleStyle->Option.getOr(Hybrid)}
        >
          {React.string(n)}
        </RuleView>
      )
      ->React.array}
    </div>
  }
}
