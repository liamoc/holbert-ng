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

  let extractConstructorSignature = (rule: Rule.t): option<(string, int)> => {
    let (head, args) = HOTerm.strip(rule.conclusion)
    switch head {
    | Symbol({name}) => Some((name, Array.length(args)))
    | _ => None
    }
  }

  let groupByConstructor = (rules: dict<Rule.t>): array<constructorGroup> => {
    Dict.toArray(rules)
    ->Array.filterMap(((name, rule)) =>
      extractConstructorSignature(rule)->Option.map(((cname, arity)) => (name, rule, cname, arity))
    )
    ->Array.reduce(Dict.make(), (acc, (name, rule, cname, arity)) => {
      let key = cname ++ "§" ++ Int.toString(arity)
      Dict.set(acc, key, [(name, rule, cname, arity), ...Dict.get(acc, key)->Option.getOr([])])
      acc
    })
    ->Dict.toArray
    ->Array.map(((_, group)) => {
      let (_, _, name, arity) = group[0]->Option.getExn
      {name, arity, rules: Array.map(group, ((n, r, _, _)) => (n, r))}
    })
  }
  let generateInductionRule = (
    group: constructorGroup,
    allGroups: array<constructorGroup>,
  ): Rule.t => {
    let {name: constructorName, arity, rules: constructors} = group
    let numFormers = Array.length(allGroups)
    let myIndex = mustFindIndex(allGroups, g => g.name == constructorName && g.arity == arity)

    let targetVars = Array.fromInitializer(~length=arity, i => "§" ++ Int.toString(i))
    let predicateVars = Array.fromInitializer(~length=numFormers, i => "§P" ++ Int.toString(i))
    let allVars = Array.concat(targetVars, predicateVars)

    let makeVar = idx => HOTerm.Var({idx: idx})
    let makeArgs = n => Array.fromInitializer(~length=n, i => makeVar(n - i - 1))

    let elimAssumption = {
      Rule.vars: [],
      premises: [],
      conclusion: HOTerm.app(HOTerm.Symbol({name: constructorName}), makeArgs(arity)),
    }

    let findFormerIndex = (name, arity) =>
      mustFindIndex(allGroups, g => g.name == name && g.arity == arity)

    let makeSubgoal = (constructorRule: Rule.t): Rule.t => {
      let offset = Array.length(constructorRule.vars)

      let inductiveHypotheses = Array.filterMap(constructorRule.premises, premise => {
        let (head, args) = HOTerm.strip(premise.conclusion)
        switch head {
        | Symbol({name: n}) => {
            let formerIndex = findFormerIndex(n, Array.length(args))
            if formerIndex >= 0 {
              let predicateIdx =
                offset + Array.length(premise.vars) + arity + numFormers - formerIndex - 1
              Some({
                Rule.vars: premise.vars,
                premises: premise.premises,
                conclusion: HOTerm.app(makeVar(predicateIdx), args),
              })
            } else {
              None
            }
          }
        | _ => None
        }
      })

      let predicateIdx = offset + arity + numFormers - myIndex - 1
      let subgoalConclusion = HOTerm.app(makeVar(predicateIdx), [constructorRule.conclusion])

      {
        Rule.vars: constructorRule.vars,
        premises: Array.concat(inductiveHypotheses, constructorRule.premises),
        conclusion: subgoalConclusion,
      }
    }

    let subgoals = Array.map(constructors, ((_, rule)) => makeSubgoal(rule))
    let conclusion = HOTerm.app(makeVar(arity + numFormers - myIndex - 1), makeArgs(arity))

    {Rule.vars: allVars, premises: [elimAssumption, ...subgoals], conclusion}
  }

  let derived = (state: state): state => {
    let groups = groupByConstructor(state)
    groups
    ->Array.map(group => {
      let name = "§induction-" ++ group.name ++ "§" ++ Int.toString(group.arity)
      let rule = generateInductionRule(group, groups)
      (name, rule)
    })
    ->Dict.fromArray
  }
  let serialise = (state: state) => {
    state
    ->Dict.toArray
    ->Array.map(((k, r)) => r->Rule.prettyPrintTopLevel(~name=k))
    ->Array.join("\n")
  }
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
    ret.contents->Result.map(state => (state, {Ports.facts: state, ruleStyle: None}))
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
