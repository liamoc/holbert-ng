open Zora
open StringTerm

module Util = TestUtil.MakeTerm(StringTerm)

zoraBlock("parse", t => {
  t->block("empty", t => t->Util.testParse(``, []))
  t->block("string literal", t => {
    t->Util.testParse(`""`, [String("")])
    t->Util.testParse(`"x"`, [String("x")])
    t->Util.testParse(`"xyz. 123"`, [String("xyz. 123")])
  })
  t->block("variables", t => {
    t->Util.testParse("\\1", [Var({idx: 1})])
    t->Util.testParse("\\10", [Var({idx: 10})])
    t->Util.testParse(`x`, ~scope=["x"], [Var({idx: 0})])
    t->Util.testParse("\\?1()", [Schematic({schematic: 1, allowed: []})])
    t->Util.testParseFail("\\?1")
    t->Util.testParse("\\?10()", [Schematic({schematic: 10, allowed: []})])
    t->Util.testParse("\\?1(1 23 4)", [Schematic({schematic: 1, allowed: [1, 23, 4]})])
  })
  t->block("flat concat", t => {
    t->Util.testParse(`"x"."y"`, [String("x"), String("y")])
    t->Util.testParse(
      `"x".\\?1(1 2 3).\\1.y`,
      ~scope=["y"],
      [String("x"), Schematic({schematic: 1, allowed: [1, 2, 3]}), Var({idx: 1}), Var({idx: 0})],
    )
    // can't end in concat
    t->Util.testParseFail(`"x".`)
  })
  t->block("paren", t => {
    // empty
    t->Util.testParse(`()`, [])
    // balanced single
    t->Util.testParse(`("x")`, [String("x")])
    t->Util.testParse(`(("x"))`, [String("x")])
    // unbalanced single
    t->Util.testParseFail(`("x"))`)
    t->Util.testParseFail(`(("x")`)
    // multi
    t->Util.testParse(`"x".("y"."z")`, [String("x"), String("y"), String("z")])
    t->Util.testParse(`("x"."y")."z"`, [String("x"), String("y"), String("z")])
    // can't concat
    t->Util.testParseFail(`("x".)."y"`)
  })
})

let parse = (input: string) =>
  StringTerm.parse(input, ~scope=[], ~gen=StringTerm.makeGen())->Result.getExn->fst

zoraBlock("unify", t => {
  let a = parse(`"a"`)
  let b = parse(`"b"`)
  let x = parse(`\\?1()`)
  t->Util.testUnify(a, a, [Map.make()])
  t->Util.testUnify(x, a, [Map.fromArray([(1, a)])])
  t->Util.testUnify(a, x, [Map.fromArray([(1, a)])])
  // no solutions because we naively require schematics
  // on at most one side for now
  t->Util.testUnify(x, x, [])

  let xy = parse(`\\?1().\\?2()`)
  let ab = parse(`"a"."b"`)
  t->Util.testUnify(x, ab, [Map.fromArray([(1, ab)])])
  t->Util.testUnify(
    xy,
    ab,
    [
      Map.fromArray([(1, []), (2, ab)]),
      Map.fromArray([(1, a), (2, b)]),
      Map.fromArray([(1, ab), (2, [])]),
    ],
  )

  t->Util.testUnify(parse(`\\?1()."b".\\?2()`), ab, [Map.fromArray([(1, a), (2, [])])])
  t->Util.testUnify(
    parse(`\\?1().\\?2()."b"`),
    ab,
    [Map.fromArray([(1, []), (2, a)]), Map.fromArray([(1, a), (2, [])])],
  )
  t->Util.testUnify(
    parse(`"a".\\?1().\\?2()`),
    ab,
    [Map.fromArray([(1, []), (2, b)]), Map.fromArray([(1, b), (2, [])])],
  )

  let xax = parse(`\\?1()."a".\\?1()`)
  t->Util.testUnify(xax, parse(`"a"`), [Map.fromArray([(1, [])])])
  t->Util.testUnify(xax, parse(`"a"."a"."a"`), [Map.fromArray([(1, a)])])
  t->Util.testUnify(xax, parse(`"a"."b"."a"."a"."b"`), [Map.fromArray([(1, parse(`"a"."b"`))])])
})
