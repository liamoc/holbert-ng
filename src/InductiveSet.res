open Signatures
open Component
open Util

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

  type constructorGroup = {
    name: string,
    arity: int,
    rules: array<(string, Rule.t)>,
  }

  let makeKey = (name, arity) => name ++ "§" ++ Int.toString(arity)

  let extractConstructorSignature = (rule: Rule.t): option<(string, int)> => {
    let (head, args) = HOTerm.strip(rule.conclusion)
    switch head {
    | Symbol({name}) => Some((name, Array.length(args)))
    | _ => None
    }
  }

  let groupByConstructor = (rules: dict<Rule.t>): array<constructorGroup> =>
    rules
    ->Dict.toArray
    ->Array.filterMap(((name, rule)) =>
      extractConstructorSignature(rule)->Option.map(sig => (name, rule, sig))
    )
    // Only allow type constructors with arity 1
    ->Array.filter(((_, _, (_cname, arity))) => arity == 1)
    ->Array.reduce(Dict.make(), (acc, (name, rule, (cname, arity))) => {
      let key = makeKey(cname, arity)
      Dict.set(acc, key, [(name, rule), ...Dict.get(acc, key)->Option.getOr([])])
      acc
    })
    ->Dict.valuesToArray
    ->Array.map(constructors => {
      let (_, firstRule) = constructors[0]->Option.getExn
      let (name, arity) = extractConstructorSignature(firstRule)->Option.getExn
      {name, arity, rules: constructors}
    })
  let generateInductionRule = (
    group: constructorGroup,
    allGroups: array<constructorGroup>,
  ): Rule.t => {
    let {name: str, arity: i} = group
    let numFormers = Array.length(allGroups)
    let groupIndex = mustFindIndex(allGroups, g => g.name == str && g.arity == i)

    let findFormerIndex = (name, arity) =>
      mustFindIndex(allGroups, g => g.name == name && g.arity == arity)

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
      | _ => raise(Unreachable("Constructor conclusion must have a Symbol head"))
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
        Array.fromInitializer(~length=i, i => "§" ++ Int.toString(i)),
        Array.fromInitializer(~length=numFormers, i => "§P" ++ Int.toString(i)),
      ),
      premises: [
        {
          Rule.vars: [],
          premises: [],
          conclusion: HOTerm.app(HOTerm.Symbol({name: str, constructor: false}), HOTerm.mkvars(i)),
        },
        ...subgoals,
      ],
      conclusion: HOTerm.app(HOTerm.Var({idx: i + groupIndex}), HOTerm.mkvars(i)),
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

  let isSelfReference = (group: constructorGroup, (name, arity)): bool =>
    name == group.name && arity == group.arity

  let findDependencies = (group: constructorGroup): array<(string, int)> =>
    group.rules
    ->Array.flatMap(((_name, rule)) => rule.premises->Array.filterMap(extractInductiveType))
    ->Array.filter(dep => !isSelfReference(group, dep))

  let rec collectReachable = (
    toVisit: array<(string, int)>,
    visited: Belt.Set.t<string, StringCmp.identity>,
    allGroups: array<constructorGroup>,
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
    targetGroup: constructorGroup,
    allGroups: array<constructorGroup>,
  ): array<constructorGroup> => {
    let reachableKeys = collectReachable(
      [(targetGroup.name, targetGroup.arity)],
      Belt.Set.make(~id=module(StringCmp)),
      allGroups,
    )
    allGroups->Array.filter(g => Belt.Set.has(reachableKeys, makeKey(g.name, g.arity)))
  }

  let generateCasesRule = (group: constructorGroup): Rule.t => {
    let {name: str, arity: _i} = group

    let caseSubgoal = ((_constructorName: string, constructorRule: Rule.t)): Rule.t => {
      let offset = Array.length(constructorRule.vars)

      // Extract the argument from the constructor conclusion
      // e.g., from (Nat 0) extract 0, from (Nat (S n)) extract (S n)
      let (_head, args) = HOTerm.strip(constructorRule.conclusion)
      let constructorArg = switch args[0] {
      | Some(arg) => arg
      | None => raise(Unreachable("Constructor conclusion must have one argument"))
      }

      let equalityPremise = {
        Rule.vars: [],
        premises: [],
        conclusion: HOTerm.app(
          HOTerm.Symbol({name: "=", constructor: false}),
          [HOTerm.Var({idx: offset}), constructorArg],
        ),
      }

      {
        Rule.vars: constructorRule.vars,
        premises: Array.concat([equalityPremise], constructorRule.premises),
        conclusion: HOTerm.Var({idx: offset + 1}),
      }
    }

    let subgoals = Array.map(group.rules, ((name, rule)) => caseSubgoal((name, rule)))

    {
      Rule.vars: Array.concat(["§0"], ["§P"]),
      premises: [
        {
          Rule.vars: [],
          premises: [],
          conclusion: HOTerm.app(
            HOTerm.Symbol({name: str, constructor: false}),
            [HOTerm.Var({idx: 0})],
          ),
        },
        ...subgoals,
      ],
      conclusion: HOTerm.Var({idx: 1}),
    }
  }

  let derived = (state: state): state =>
    state
    ->groupByConstructor
    ->Array.flatMap(group => {
      let mutualComponent = findMutuallyInductiveComponent(group, groupByConstructor(state))
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
      )}>
      {Dict.toArray(props.content->Dict.copy->Dict.assign(derived(props.content)))
      ->Array.mapWithIndex(((n, r), i) =>
        <RuleView
          rule={r}
          scope={[]}
          key={String.make(i)}
          style={props.imports.ruleStyle->Option.getOr(Hybrid)}>
          {React.string(n)}
        </RuleView>
      )
      ->React.array}
    </div>
  }
}
