open Zora

module Util = TestUtil.MakeAtomTester(Preludic.Atom)
module ParseUtil = Util.ParseTester

zoraBlock("parse prelude", t =>
  t->ParseUtil.testParse(
    `length("$s")`,
    ~scope=["s"],
    Preludic.Base.wrapForReal(
      AtomBase.AnyValue(Preludic.LengthTag, [AtomBase.String.Var({idx: 0})]),
    ),
  )
)
