open Zora

module NatBase = AssocCommBase.Nat
module Nat = AssocComm.Nat.Atom
module Util = TestUtil.MakeAtomTester(Nat)
module ParseUtil = Util.ParseTester
module UnifyUtil = Util.UnifyTester

module IntMap = Belt.Map.Int
let const = NatBase.const
let var = NatBase.var
let schematic = NatBase.schematic

zoraBlock("parse nat", t => {
  let testParse = (t, str, expected, ~scope=?) =>
    t->ParseUtil.testParse(`nat(${str})`, expected, ~scope?)
  let testParseFail = (t, str, ~scope=?) => t->ParseUtil.testParseFail(`nat(${str})`, ~scope?)
  t->block("const", t => {
    t->block("single digit", t => t->testParse("1", const(1)))
    t->block("multi digit", t => t->testParse("123", const(123)))
    t->block("negative", t => t->testParseFail("-1"))
  })
  t->block("var", t => {
    t->block("in scope", t => t->testParse("x", var(0), ~scope=["x"]))
    t->block("not in scope", t => t->testParseFail("x"))
    t->block("const", t => t->testParse(`\\1`, var(1)))
    t->block("const multi digit", t => t->testParse(`\\10`, var(10)))
  })
  t->block("schematic", t => {
    t->block("valid empty allowed", t => t->testParse(`?1()`, schematic(1, [])))
    t->block("invalid no allowed", t => t->testParseFail(`?1`))
    t->block("valid empty allowed multi digit", t => t->testParse(`?10()`, schematic(10, [])))
    t->block(
      "valid with non-empty allowed",
      t => t->testParse(`?10(\\1 \\23 \\4)`, schematic(10, [1, 23, 4])),
    )
  })
  t->block("single add", t => t->testParse("1 + 2", Nat.add(const(1), const(2))))
  t->block("multi add", t => {
    t->testParse("1 + 2 + x", Nat.add(Nat.add(const(1), const(2)), var(0)), ~scope=["x"])
    t->testParse("?1(x) + ?1()", Nat.add(schematic(1, [0]), schematic(1, [])), ~scope=["x"])
  })
})

zoraBlock("unify nat", t => {
  let parse = s => Nat.parse(`nat(${s})`, ~scope=[])->Result.getExn->Pair.first
  t->block("test", t => t->UnifyUtil.testUnify(parse("?1()"), parse("?2()")))
  t->block("test", t => t->UnifyUtil.testUnify(parse("?1() + ?2()"), parse("?3() + ?3()")))
})
