open Component

module Term = StringTerm
module Judgment = StringTermJudgment
module JudgmentView = StringTermJView

module Rule = Rule.Make(Term, Judgment)
module RuleView = RuleView.Make(Term, Judgment, JudgmentView)
module Ports = Ports(Term, Judgment)
type state = {
  raw: dict<Rule.t>,
  derived: dict<Rule.t>,
}

type props = {
  content: state,
  imports: Ports.t,
  onChange: (state, ~exports: Ports.t=?) => unit,
}

module Set = Belt.Set.String
let varsInRule = (rule: Rule.t) => {
  rule.premises->Array.reduce(Set.fromArray(rule.vars), (s, r) =>
    s->Set.union(Set.fromArray(r.vars))
  )
}

let derive = (name: string, rules: array<Rule.t>): Rule.t => {
  // TODO: can we remove some of the hardcoding?
  let allVars = rules->Array.reduce(Set.empty, (s, r) => s->Set.union(varsInRule(r)))
  let rec genVar = (base: string) => {
    if allVars->Set.has(base) {
      genVar(`${base}'`)
    } else {
      base
    }
  }
  let (p, b, x, a) = (genVar("P"), genVar("b"), genVar("x"), genVar("a"))
  let vars = [p, b, x, a]
  let xIdx = vars->Array.findIndex(i => i == x)
  let aIdx = vars->Array.findIndex(i => i == a)
  let bIdx = vars->Array.findIndex(i => i == b)
  let pIdx = vars->Array.findIndex(i => i == p)
  let surround = (t: StringTerm.t, aIdx: int, bIdx: int) => {
    Array.concat(Array.concat([StringTerm.Var({idx: aIdx})], t), [StringTerm.Var({idx: bIdx})])
  }
  let rec replaceJudgeRHS = (rule: Rule.t, aIdx: int, bIdx: int, pIdx: int): Rule.t => {
    let n = Array.length(rule.vars)
    let (aIdx, bIdx, pIdx) = (aIdx + n, bIdx + n, pIdx + n)
    let inductionHyps =
      rule.premises
      ->Array.filter(r => r.conclusion->snd == Symbol({name: name}))
      ->Array.map(r => replaceJudgeRHS(r, aIdx, bIdx, pIdx))
    {
      vars: rule.vars,
      premises: rule.premises->Array.concat(inductionHyps),
      conclusion: (surround(rule.conclusion->fst, aIdx, bIdx), Var({idx: pIdx})),
    }
  }
  {
    vars,
    premises: Array.concat(
      [
        {
          Rule.vars: [],
          premises: [],
          conclusion: ([StringTerm.Var({idx: xIdx})], Symbol({name: name})),
        },
      ],
      rules->Array.map(r => replaceJudgeRHS(r, aIdx, bIdx, pIdx)),
    ),
    conclusion: (surround([StringTerm.Var({idx: xIdx})], aIdx, bIdx), Var({idx: pIdx})),
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
      switch rule.conclusion->snd {
      | Symbol({name: a}) =>
        switch grouped->Dict.get(a) {
        | None => grouped->Dict.set(a, [rule])
        | Some(rs) => rs->Array.push(rule)
        }
      | _ => ()
      }
    )
    let derived: Dict.t<Rule.t> = Dict.make()
    grouped->Dict.forEachWithKey((rules, name) => {
      // NOTE: this can clash with other names. is this an issue?
      derived->Dict.set(`${name}_induct`, derive(name, rules))
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
      )}>
      {content
      ->Dict.toArray
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
  <div>
    {makeRules(props.content.raw)}
    <p> {React.string("derived")} </p>
    {makeRules(props.content.derived)}
  </div>
}
