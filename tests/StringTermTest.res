open Zora
open StringTerm

module Util = TestUtil.MakeTerm(StringTerm)

zoraBlock("parse", t => {
  t->block("empty", t => t->Util.testParse(`""`, []))
  t->block("string literal", t => {
    t->Util.testParse(`"x"`, [String("x")])
    t->Util.testParse(`"xyz123"`, [String("xyz123")])
    t->Util.testParse(`"123y"`, [String("123"), String("y")])
    t->Util.testParse(`"\\"\\$\\?\\\\"`, [String("\""), String("$"), String("?"), String("\\")])
    t->Util.testParse(
      `"y(135ab!!)"`,
      [String("y"), String("("), String("135"), String("ab"), String("!!"), String(")")],
    )
    t->Util.testParseFail(`foo`)
    t->Util.testParseFail(`a b" c`)
  })
  t->block("variables", t => {
    t->Util.testParse(`"$\\1"`, [Var({idx: 1})])
    t->Util.testParse(`"$\\10"`, [Var({idx: 10})])
    t->Util.testParse(`"$x"`, ~scope=["x"], [Var({idx: 0})])
    t->Util.testParse(`"?1()"`, [Schematic({schematic: 1, allowed: []})])
    t->Util.testParseFail(`"?1"`)
    t->Util.testParse(`"?10()"`, [Schematic({schematic: 10, allowed: []})])
    t->Util.testParse(`"?1(1 23 4)"`, [Schematic({schematic: 1, allowed: [1, 23, 4]})])
  })
  t->block("concat", t => {
    t->Util.testParse(`"x y"`, [String("x"), String("y")])
    t->Util.testParse(`"x  y"`, [String("x"), String("y")])
    t->Util.testParse(
      `"x ?1(1 2 3) $\\1 $y"`,
      ~scope=["y"],
      [String("x"), Schematic({schematic: 1, allowed: [1, 2, 3]}), Var({idx: 1}), Var({idx: 0})],
    )
  })
})

let parse = (input: string) =>
  StringTerm.parse(input, ~scope=[], ~gen=StringTerm.makeGen())->Result.getExn->fst

zoraBlock("unify", t => {
  let a = parse(`"a"`)
  let b = parse(`"b"`)
  let x = parse(`"?1()"`)
  let y = parse(`"?2()"`)
  t->block("schematics on at most one side", t => {
    t->Util.testUnify(a, a, [Map.make()])
    t->Util.testUnify(x, a, [Map.fromArray([(1, a)])])
    t->Util.testUnify(a, x, [Map.fromArray([(1, a)])])

    let xy = parse(`"?1() ?2()"`)
    let ab = parse(`"a b"`)
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

    t->Util.testUnify(parse(`"?1() b ?2()"`), ab, [Map.fromArray([(1, a), (2, [])])])
    t->Util.testUnify(
      parse(`"?1() ?2() b"`),
      ab,
      [Map.fromArray([(1, []), (2, a)]), Map.fromArray([(1, a), (2, [])])],
    )
    t->Util.testUnify(
      parse(`"a ?1() ?2()"`),
      ab,
      [Map.fromArray([(1, []), (2, b)]), Map.fromArray([(1, b), (2, [])])],
    )

    let xax = parse(`"?1() a ?1()"`)
    t->Util.testUnify(xax, parse(`"a"`), [Map.fromArray([(1, [])])])
    t->Util.testUnify(xax, parse(`"a a a"`), [Map.fromArray([(1, a)])])
    t->Util.testUnify(xax, parse(`"a b a a b"`), [Map.fromArray([(1, parse(`"a b"`))])])
  })

  t->block("schematics appearing at most twice", t => {
    t->Util.testUnify(x, x, [Map.fromArray([(1, [])])])
    t->Util.testUnify(x, y, [Map.fromArray([(1, []), (2, [])])])

    t->Util.testUnify(a, parse(`"?1() a"`), [Map.fromArray([(1, [])])])
    t->Util.testUnify(
      parse(`"?1() a"`),
      parse(`"a ?1()"`),
      [
        Map.fromArray([(1, [])]),
        Map.fromArray([(1, parse(`"a"`))]),
        Map.fromArray([(1, parse(`"a a"`))]),
        Map.fromArray([(1, parse(`"a a a"`))]),
        Map.fromArray([(1, parse(`"a a a a"`))]),
      ],
    )
    t->Util.testUnify(parse(`"a ?1()"`), parse(`"?2() b`), [Map.fromArray([(1, b), (2, a)])])
    t->Util.testUnify(parse(`"a ?1() a"`), parse(`"?2() b a"`), [Map.fromArray([(1, b), (2, a)])])
    t->Util.testUnify(
      parse(`"b ?1() a"`),
      parse(`"?2() a ?1()"`),
      [Map.fromArray([(1, a), (2, b)]), Map.fromArray([(1, []), (2, b)])],
    )
    t->Util.testUnify(
      parse(`"a b ?1() c ?2()"`),
      parse(`"?2() c ?1() b a"`),
      [Map.fromArray([(1, a), (2, parse(`"a b a"`))])],
    )
  })
})
