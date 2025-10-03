open Zora
open SExp

module Util = TestUtil.MakeTerm(SExp)

zoraBlock("parse symbol", t => {
  t->block("single char", t => t->Util.testParse("x", Symbol({name: "x"})))
  t->block("multi char", t => t->Util.testParse("xyz", Symbol({name: "xyz"})))
  t->block("judgement terminal", t =>
    t->Util.testParse("a]", Symbol({name: "a"}), ~expectRemaining="]")
  )
})

zoraBlock("parse var", t => {
  t->block("single digit", t => t->Util.testParse("\\1", Var({idx: 1})))
  t->block("multi digit", t => t->Util.testParse("\\234", Var({idx: 234})))
  t->block("scope", t => t->Util.testParse("p", ~scope=["q", "p"], Var({idx: 1})))
})

zoraBlock("parse schematic", t => {
  t->block("empty allowed", t => t->Util.testParse("?1()", Schematic({schematic: 1, allowed: []})))
  t->block("one allowed", t =>
    t->Util.testParse("?1(\\1)", Schematic({schematic: 1, allowed: [1]}))
  )
  t->block("multiple allowed", t =>
    t->Util.testParse("?1(\\1 \\23 \\4)", Schematic({schematic: 1, allowed: [1, 23, 4]}))
  )
})

zoraBlock("parse compound", t => {
  t->block("unit", t => t->Util.testParse("()", Compound({subexps: []})))
  t->block("single", t => t->Util.testParse("(a)", Compound({subexps: [Symbol({name: "a"})]})))
  t->block("multiple", t => {
    t->Util.testParse(
      "(a \\1 ?1())",
      Compound({
        subexps: [Symbol({name: "a"}), Var({idx: 1}), Schematic({schematic: 1, allowed: []})],
      }),
    )
  })
})

let parse = (input: string) => SExp.parse(input, ~scope=[], ~gen=SExp.makeGen())->Result.getExn->fst

zoraBlock("unify var", t => {
  let x = parse("x")
  let y = parse("y")
  let comp1 = parse("(x y z)")
  let comp2 = parse("(x a y)")
  let schema1 = parse("?1()")
  let schemaComp = parse("(?1() a ?2())")
  t->block("var eq", t => t->Util.testUnify(x, x, [Map.make()]))
  t->block("var neq", t => t->Util.testUnify(x, y, []))
  t->block("comp eq", t => t->Util.testUnify(comp1, comp1, [Map.make()]))
  t->block("comp neq", t => t->Util.testUnify(comp1, comp2, []))
  t->block("schema var", t => t->Util.testUnify(schema1, x, [Map.fromArray([(1, x)])]))
  t->block("schema comp", t => t->Util.testUnify(schema1, comp2, [Map.fromArray([(1, comp2)])]))
  t->block("comp-schema comp eq", t => {
    t->Util.testUnify(schemaComp, comp2, [Map.fromArray([(1, x), (2, y)])])
  })
  t->block("comp-schema comp neq", t => {
    t->Util.testUnify(schemaComp, comp1, [])
  })
})
