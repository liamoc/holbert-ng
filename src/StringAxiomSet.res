open Component

module StringSymbol = AtomDef.MakeAtomAndView(
  Coercible.StringA,
  StringA.AtomView,
  Coercible.Symbolic,
  Symbolic.AtomView,
)
module StringSExp = SExp.Make(StringSymbol.Atom)
module TermView = SExpView.Make(StringSymbol.Atom, StringSymbol.AtomView, StringSExp)
module JudgmentView = TermViewAsJudgmentView.Make(StringSExp, StringSExp, TermView)

module Rule = Rule.Make(StringSExp, StringSExp)
module RuleView = RuleView.Make(StringSExp, StringSExp, JudgmentView)
module Ports = Ports(StringSExp, StringSExp)
type state = {
  raw: dict<Rule.t>,
  derived: dict<Rule.t>,
}

type props = {
  content: state,
  imports: Ports.t,
  onChange: (state, ~exports: Ports.t=?) => unit,
}

type judgeGroup = {
  name: string,
  rules: array<Rule.t>,
}

module Set = Belt.Set.String
let varsInRule = (rule: Rule.t) => {
  rule.premises->Array.reduce(Set.fromArray(rule.vars), (s, r) =>
    s->Set.union(Set.fromArray(r.vars))
  )
}

let getSExpName = (t: StringSExp.t): option<string> =>
  switch t {
  | Atom(name) => Some(name->StringSymbol.Atom.prettyPrint(~scope=[]))
  | _ => None
  }

let destructure = (r: StringSExp.t): (StringA.Atom.t, string) =>
  switch r {
  | Compound({subexps: [Atom(Left(s)), Atom(Right(name))]}) => (s, name)
  | _ => throw(Util.Unreachable("expected valid induction rule"))
  }
let destructureOpt = (r: StringSExp.t): option<(StringA.Atom.t, string)> =>
  switch r {
  | Compound({subexps: [Atom(Left(s)), Atom(Right(name))]}) => Some((s, name))
  | _ => None
  }
let structure = (lhs: StringSExp.t, rhs: StringSExp.t): StringSExp.t => Compound({
  subexps: [lhs, rhs],
})
let findMentionedRuleGroups = (group: judgeGroup, allGroups: array<judgeGroup>): array<
  judgeGroup,
> => {
  let allGroupNames = allGroups->Array.map(g => g.name)
  let groupNames: array<string> = Array.concat(
    [group.name],
    group.rules
    ->Array.flatMap(r =>
      r.premises
      ->Array.filterMap(p => p.conclusion->destructureOpt->Option.map(Pair.second))
      ->Array.filter(name => allGroupNames->Array.find(name' => name' == name)->Option.isSome)
    )
    ->Set.fromArray
    ->Set.remove(group.name)
    ->Set.toArray,
  )
  groupNames->Array.map(name => allGroups->Array.find(g => g.name == name))->Array.keepSome
}

let derive = (group: judgeGroup, mentionedGroups: array<judgeGroup>): Rule.t => {
  let allVars =
    mentionedGroups
    ->Array.flatMap(g => g.rules)
    ->Array.reduce(Set.empty, (s, r) => s->Set.union(varsInRule(r)))
  let rec genVar = (base: string) => {
    if allVars->Set.has(base) {
      genVar(`${base}'`)
    } else {
      base
    }
  }
  let ps = mentionedGroups->Array.map(g => genVar(`P${g.name}`))
  let (b, x, a) = (genVar("b"), genVar("x"), genVar("a"))
  let vars = Array.concat(ps, [b, x, a])
  let xIdx = vars->Array.findIndex(i => i == x)
  let aIdx = vars->Array.findIndex(i => i == a)
  let bIdx = vars->Array.findIndex(i => i == b)
  let surround = (t: StringA.Atom.t, aIdx: int, bIdx: int) => {
    Array.concat(Array.concat([StringA.Var({idx: aIdx})], t), [StringA.Var({idx: bIdx})])
  }
  let lookupGroup = (name: string): option<int> =>
    mentionedGroups->Array.findIndexOpt(g => name == g.name)
  let rec replaceJudgeRHS = (rule: Rule.t, baseIdx: int): Rule.t => {
    let baseIdx = baseIdx + Array.length(rule.vars)
    let (s, name) = destructure(rule.conclusion)
    let inductionHyps =
      rule.premises
      ->Array.filter(r =>
        r.conclusion
        ->destructureOpt
        ->Option.flatMap(conclusion => conclusion->Pair.second->lookupGroup)
        ->Option.isSome
      )
      ->Array.map(r => replaceJudgeRHS(r, baseIdx))
    let pIdx = lookupGroup(name)->Option.getExn
    {
      vars: rule.vars,
      premises: rule.premises->Array.concat(inductionHyps),
      conclusion: structure(
        surround(s, aIdx + baseIdx, bIdx + baseIdx)->Left->Atom,
        Var({idx: pIdx + baseIdx}),
      ),
    }
  }
  {
    vars,
    premises: Array.concat(
      [
        {
          Rule.vars: [],
          premises: [],
          conclusion: structure(Var({idx: xIdx}), Atom(Right(group.name))),
        },
      ],
      mentionedGroups->Array.flatMap(g => g.rules->Array.map(r => replaceJudgeRHS(r, 0))),
    ),
    conclusion: structure(
      surround([StringA.Var({idx: xIdx})], aIdx, bIdx)->Left->Atom,
      Var({idx: 0}),
    ), // TODO: clean here
  }
}

let deserialise = (str: string, ~imports as _: Ports.t) => {
  let getBase = (str: string) => {
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
    ret.contents
  }
  getBase(str)->Result.map(raw => {
    let grouped: dict<array<Rule.t>> = Dict.make()
    raw->Dict.forEach(rule =>
      switch rule.conclusion {
      | Compound({subexps: [Atom(Left(_)), Atom(Right(name))]}) =>
        switch grouped->Dict.get(name) {
        | None => grouped->Dict.set(name, [rule])
        | Some(rs) => rs->Array.push(rule)
        }
      | _ => ()
      }
    )
    let allGroups = grouped->Dict.toArray->Array.map(((name, rules)) => {name, rules})
    let derived: Dict.t<Rule.t> = Dict.make()
    allGroups->Array.forEach(group => {
      // NOTE: this can clash with other names. is this an issue?
      derived->Dict.set(`${group.name}_induct`, derive(group, [group]))
      let mentionedGroups = findMentionedRuleGroups(group, allGroups)
      if mentionedGroups->Array.length > 1 {
        derived->Dict.set(`${group.name}_mutualInduct`, derive(group, mentionedGroups))
      }
    })
    ({raw, derived}, {Ports.facts: raw->Dict.copy->Dict.assign(derived), ruleStyle: None})
  })
}

let serialise = (state: state) => {
  state.raw
  ->Dict.toArray
  ->Array.map(((k, r)) => r->Rule.prettyPrintTopLevel(~name=k))
  ->Array.join("\n")
}

let make = props => {
  let makeRules = content =>
    <div
      className={"axiom-set axiom-set-"->String.concat(
        String.make(props.imports.ruleStyle->Option.getOr(Hybrid)),
      )}
    >
      {content
      ->Dict.toArray
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
  <div>
    {makeRules(props.content.raw)}
    <p> {React.string("derived")} </p>
    {makeRules(props.content.derived)}
  </div>
}
