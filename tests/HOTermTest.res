open Zora

module Util = TestUtil.MakeTerm(HOTerm)


let parse = (input: string) =>
  HOTerm.parse(input, ~scope=[], ~gen=HOTerm.makeGen())->Result.getExn->Pair.first

zoraBlock("unification", t => {
  let x = parse("(Implies A B)")
  let y = parse("(Implies ?0 ?1)")
  t->Util.testUnify(x, y, ~expect=[Belt.Map.Int.fromArray([(0, parse("A")), (1, parse("B"))])])
  
})
