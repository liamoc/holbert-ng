open Zora
open HOTerm

module Util = TestUtil.MakeTerm(HOTerm)

zoraBlock("parse symbol", t => {
  t->block("single char", t => t->Util.testParse("x", Symbol({name: "x"})))
  t->block("multi char", t => t->Util.testParse("xyz", Symbol({name: "xyz"})))
})

zoraBlock("parse var", t => {
  t->block("single digit", t => t->Util.testParse("\\1", Var({idx: 1})))
  t->block("multi digit", t => t->Util.testParse("\\234", Var({idx: 234})))
})

zoraBlock("parse application", t => {
  t->block("multiple", t => {
    t->Util.testParse("(a b)", App({func: Symbol({name: "a"}), arg: Symbol({name: "b"})}))
  })
  t->block("multiple more", t => {
    t->Util.testParse(
      "(a b c)",
      App({
        func: App({func: Symbol({name: "a"}), arg: Symbol({name: "b"})}),
        arg: Symbol({name: "c"}),
      }),
    )
  })
})
