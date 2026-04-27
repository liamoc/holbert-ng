open Zora

module Util = TestUtil.MakeAtomTester(StringA.Atom)
module ParseUtil = Util.ParseTester

zoraBlock("parse", t => {
  t->block("empty", t => t->ParseUtil.testParse(`""`, []))
  t->block("string literal", t => {
    t->ParseUtil.testParse(`"x"`, [String("x")])
    t->ParseUtil.testParse(`"xyz123"`, [String("xyz123")])
    t->ParseUtil.testParse(`"123y"`, [String("123"), String("y")])
    t->ParseUtil.testParse(
      `"\\"\\$\\?\\\\"`,
      [String("\""), String("$"), String("?"), String("\\")],
    )
    t->ParseUtil.testParse(
      `"y(135ab!!)"`,
      [String("y"), String("("), String("135"), String("ab"), String("!!"), String(")")],
    )
    t->ParseUtil.testParseFail(`foo`)
    t->ParseUtil.testParseFail(`a b" c`)
  })
  t->block("variables", t => {
    t->ParseUtil.testParse(`"$\\1"`, [Var({idx: 1})])
    t->ParseUtil.testParse(`"$\\10"`, [Var({idx: 10})])
    t->ParseUtil.testParse(`"$x"`, ~scope=["x"], [Var({idx: 0})])
    t->ParseUtil.testParse(`"?1()"`, [Schematic({schematic: 1, allowed: []})])
    t->ParseUtil.testParseFail(`"?1"`)
    t->ParseUtil.testParse(`"?10()"`, [Schematic({schematic: 10, allowed: []})])
    t->ParseUtil.testParse(`"?1(1 23 4)"`, [Schematic({schematic: 1, allowed: [1, 23, 4]})])
  })
  t->block("concat", t => {
    t->ParseUtil.testParse(`"x y"`, [String("x"), String("y")])
    t->ParseUtil.testParse(`"x  y"`, [String("x"), String("y")])
    t->ParseUtil.testParse(
      `"x ?1(1 2 3) $\\1 $y"`,
      ~scope=["y"],
      [String("x"), Schematic({schematic: 1, allowed: [1, 2, 3]}), Var({idx: 1}), Var({idx: 0})],
    )
  })
})

let parse = (input: string) =>
  StringA.Atom.parse(input, ~scope=[], ~gen=Util.ParseWrapper.makeGen())->Result.getExn->Pair.first

module UnifyUtil = Util.UnifyTester

zoraBlock("unify", t => {
  let a = parse(`"a"`)
  let b = parse(`"b"`)
  let x = parse(`"?1()"`)
  let y = parse(`"?2()"`)
  t->block("schematics on at most one side", t => {
    t->UnifyUtil.testUnify(a, a, ~expect=[Map.make()])
    t->UnifyUtil.testUnify(x, a, ~expect=[Map.fromArray([(1, a)])])
    t->UnifyUtil.testUnify(a, x, ~expect=[Map.fromArray([(1, a)])])

    let xy = parse(`"?1() ?2()"`)
    let ab = parse(`"a b"`)
    t->UnifyUtil.testUnify(x, ab, ~expect=[Map.fromArray([(1, ab)])])
    t->UnifyUtil.testUnify(
      xy,
      ab,
      ~expect=[
        Map.fromArray([(1, []), (2, ab)]),
        Map.fromArray([(1, a), (2, b)]),
        Map.fromArray([(1, ab), (2, [])]),
      ],
    )

    t->UnifyUtil.testUnify(parse(`"?1() b ?2()"`), ab, ~expect=[Map.fromArray([(1, a), (2, [])])])
    t->UnifyUtil.testUnify(
      parse(`"?1() ?2() b"`),
      ab,
      ~expect=[Map.fromArray([(1, []), (2, a)]), Map.fromArray([(1, a), (2, [])])],
    )
    t->UnifyUtil.testUnify(
      parse(`"a ?1() ?2()"`),
      ab,
      ~expect=[Map.fromArray([(1, []), (2, b)]), Map.fromArray([(1, b), (2, [])])],
    )

    let xax = parse(`"?1() a ?1()"`)
    t->UnifyUtil.testUnify(xax, parse(`"a"`), ~expect=[Map.fromArray([(1, [])])])
    t->UnifyUtil.testUnify(xax, parse(`"a a a"`), ~expect=[Map.fromArray([(1, a)])])
    t->UnifyUtil.testUnify(
      xax,
      parse(`"a b a a b"`),
      ~expect=[Map.fromArray([(1, parse(`"a b"`))])],
    )
  })

  t->block("schematics appearing at most twice", t => {
    t->UnifyUtil.testUnify(x, y, ~expect=[Map.fromArray([(1, y)])])
    t->UnifyUtil.testUnify(x, x, ~expect=[Map.fromArray([])])

    t->UnifyUtil.testUnify(a, parse(`"?1() a"`), ~expect=[Map.fromArray([(1, [])])])
    t->UnifyUtil.testUnify(
      parse(`"?1() a"`),
      parse(`"a ?1()"`),
      ~expect=[
        Map.fromArray([(1, [])]),
        Map.fromArray([(1, parse(`"a"`))]),
        Map.fromArray([(1, parse(`"a a"`))]),
        Map.fromArray([(1, parse(`"a a a"`))]),
        Map.fromArray([(1, parse(`"a a a a"`))]),
      ],
    )
    t->UnifyUtil.testUnify(
      parse(`"a ?1()"`),
      parse(`"?2() b`),
      ~expect=[Map.fromArray([(1, [y, b]->Array.flat), (2, [a, y]->Array.flat)])],
    )
    t->UnifyUtil.testUnify(
      parse(`"a ?1() a"`),
      parse(`"?2() b a"`),
      ~expect=[Map.fromArray([(1, [y, b]->Array.flat), (2, [a, y]->Array.flat)])],
    )
    t->UnifyUtil.testUnify(
      parse(`"b ?1() a"`),
      parse(`"?2() a ?1()"`),
      ~expect=[Map.fromArray([(1, a), (2, b)]), Map.fromArray([(1, []), (2, b)])],
    )
    t->UnifyUtil.testUnify(
      parse(`"a b ?1() c ?2()"`),
      parse(`"?2() c ?1() b a"`),
      ~expect=[Map.fromArray([(1, a), (2, parse(`"a b a"`))])],
    )
  })
})
