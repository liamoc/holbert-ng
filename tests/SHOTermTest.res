open Zora

module Util = TestUtil.MakeTerm(SHOTerm)


let parse = (input: string) =>
  SHOTerm.parse(input, ~scope=[], ~gen=SHOTerm.makeGen())->Result.getExn->Pair.first

zoraBlock("unification", t => {
  let x = parse("(Implies A B)")
  let y = parse("(Implies ?0 ?1)")
  t->Util.testUnify(x, y, ~expect=[Belt.Map.Int.fromArray([(0, parse("A")), (1, parse("B"))])])
  
})
