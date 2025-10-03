open Signatures
open Zora

module MakeTest = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module RuleInst = Rule.Make(Term, Judgment)
  let testParseInner = (t: Zora.t, input: string, expect: RuleInst.t, ~scope=[], ~msg=?) => {
    let res = RuleInst.parseInner(input, ~scope, ~gen=Term.makeGen())
    switch res {
    | Ok(res) => {
        t->equal(res->snd, "", ~msg=input ++ " input consumed")
        t->equal(res->fst, expect, ~msg?)
      }
    | Error(msg) => t->fail(~msg="parse failed: " ++ msg)
    }
  }
  let testParseTopLevel = (
    t: Zora.t,
    input: string,
    expect: (RuleInst.t, string),
    ~scope=[],
    ~msg=?,
  ) => {
    let res = RuleInst.parseTopLevel(input, ~scope, ~gen=Term.makeGen())
    switch res {
    | Ok(res) => {
        t->equal(res->snd, "", ~msg=input ++ " input consumed")
        t->equal(res->fst, expect, ~msg?)
      }
    | Error(msg) => t->fail(~msg="parse failed: " ++ msg)
    }
  }
}

zoraBlock("string terms", t => {
  module T = MakeTest(StringTerm, StringTermJudgment)
  t->T.testParseInner(
    `[s1. "$s1" p |- "($s1)" p]`,
    {
      vars: ["s1"],
      premises: [
        {
          vars: [],
          premises: [],
          conclusion: ([StringTerm.Var({idx: 0})], Symbol({name: "p"})),
        },
      ],
      conclusion: (
        [StringTerm.String("("), StringTerm.Var({idx: 0}), StringTerm.String(")")],
        SExp.Symbol({name: "p"}),
      ),
    },
  )
})
