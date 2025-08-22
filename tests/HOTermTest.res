open Zora
open HOTerm

module Util = TestUtil.MakeTerm(HOTerm)

let testUnify0 = (t: Zora.t, at: string, bt: string, ~subst=?, ~msg=?, ~reduce=false) => {
  let gen = HOTerm.makeGen()
  let (a, _) = HOTerm.parse(at, ~scope=[], ~gen)->Result.getExn
  let (b, _) = HOTerm.parse(bt, ~scope=[], ~gen)->Result.getExn
  try {
    let res0 = HOTerm.unifyTerm(a, b, HOTerm.emptySubst, ~gen=Some(gen))
    let res = if reduce {
      HOTerm.reduceSubst(res0)
    } else {
      res0
    }
    switch subst {
    | None => t->ok(true, ~msg=msg->Option.getOr("unification succeeded"))
    | Some(subst) =>
      subst->Belt.Map.Int.forEach((k, v) => {
        let expected = subst->Belt.Map.Int.getExn(k)
        if res->Belt.Map.Int.has(k) == false {
          t->fail(
            ~msg=msg->Option.getOr("substitution on " ++ TestUtil.stringifyExn(k) ++ " not found"),
          )
        } else {
          let actual = res->Belt.Map.Int.getExn(k)
          t->equal(
            actual,
            expected,
            ~msg=msg->Option.getOr("substitution on " ++ TestUtil.stringifyExn(k)),
          )
        }
      })
    }
  } catch {
  | HOTerm.UnifyFail(failed) =>
    t->fail(
      ~msg="unification failed: " ++
      TestUtil.stringifyExn(a) ++
      " and " ++
      TestUtil.stringifyExn(b) ++
      " with error: " ++
      failed,
    )
  }
}
let testUnify = (t: Zora.t, at: string, bt: string, ~subst=?, ~msg=?, ~reduce=true) => {
  testUnify0(t, at, bt, ~subst?, ~msg?, ~reduce)
  testUnify0(t, bt, at, ~subst?, ~msg?, ~reduce)
}
zoraBlock("parse symbol", t => {
  t->block("single char", t => t->Util.testParse("x", Symbol({name: "x"})))
  t->block("multi char", t => t->Util.testParse("xyz", Symbol({name: "xyz"})))
})

zoraBlock("parse var", t => {
  t->block("single digit", t => t->Util.testParse("\\1", Var({idx: 1})))
  t->block("multi digit", t => t->Util.testParse("\\234", Var({idx: 234})))
})

zoraBlock("parse schematic", t => {
  t->block("empty allowed", t => t->Util.testParse("?1", Schematic({schematic: 1})))
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
  t->block("multiple var", t => {
    t->Util.testParse(
      "(a \\1 ?1)",
      App({
        func: App({func: Symbol({name: "a"}), arg: Var({idx: 1})}),
        arg: Schematic({schematic: 1}),
      }),
    )
  })
})

zoraBlock("parse lambda", t => {
  t->block("simple", t => {
    t->Util.testParse("(x. x)", Lam({name: "x", body: Var({idx: 0})}))
  })
  t->block("with application", t => {
    t->Util.testParse(
      "(x. x x)",
      Lam({name: "x", body: App({func: Var({idx: 0}), arg: Var({idx: 0})})}),
    )
  })
  t->block("with application 2args", t => {
    t->Util.testParse(
      "(x. y. x y)",
      Lam({
        name: "x",
        body: Lam({name: "y", body: App({func: Var({idx: 1}), arg: Var({idx: 0})})}),
      }),
    )
    t->Util.testParse("(x. y. x)", Lam({name: "x", body: Lam({name: "y", body: Var({idx: 1})})}))
  })
  t->block("omit outer ()", t => {
    t->Util.testParse(
      "x. (x x)",
      Lam({name: "x", body: App({func: Var({idx: 0}), arg: Var({idx: 0})})}),
    )
  })
  // TODO: test if remaining strings are returned correctly
})

zoraBlock("parse and prettyprint", t => {
  t->block("examples", t => {
    t->Util.testParsePrettyPrint("\\1", "\\1")
    t->Util.testParsePrettyPrint("?1", "?1")
    t->Util.testParsePrettyPrint("(x. x)", "(x. x)")
    t->Util.testParsePrettyPrint("(x. x. \\0)", "(x. x. x)")
    t->Util.testParsePrettyPrint("(x. x. \\1)", "(x. x. \\1)")
    t->Util.testParsePrettyPrint("(x. y. z. z)", "(x. y. z. z)")
    t->Util.testParsePrettyPrint("(x. y. z. y)", "(x. y. z. y)")
    t->Util.testParsePrettyPrint("(x. y. z. x)", "(x. y. z. x)")
    t->Util.testParsePrettyPrint("(x. y. z. z y x)", "(x. y. z. z y x)")
  })
})

zoraBlock("unify test", t => {
  t->block("symbols", t => {
    let x = "x"
    let y = "y"
    t->testUnify(x, x)
    t->Util.testUnifyFail(y, x)
    t->Util.testUnifyFail(x, y)
  })
  t->block("applications", t => {
    let ab = "(a b)"
    let cd = "(c d)"
    t->testUnify(ab, ab)
    t->testUnify(cd, cd)
    t->Util.testUnifyFail(ab, cd)
    t->Util.testUnifyFail(cd, ab)
  })
  t->block("flex-rigid", t => {
    let x = "?0"
    let y = "y"
    t->testUnify(x, y, ~subst=emptySubst->substAdd(0, Symbol({name: "y"})))
  })
  t->block("flex-rigid2", t => {
    let x = "(x. ?0 x)"
    let y = "(x. y x)"
    // it is y only after eta reduction
    t->testUnify(
      x,
      y,
      ~reduce=false,
      ~subst=emptySubst->substAdd(
        0,
        Lam({name: "x", body: App({func: Symbol({name: "y"}), arg: Var({idx: 0})})}),
      ),
    )
    t->testUnify(x, y, ~reduce=true, ~subst=emptySubst->substAdd(0, Symbol({name: "y"})))
  })
  t->block("?0 \\0", t => {
    let x = "(?0 \\0)"
    let y = "\\0"
    t->testUnify(x, y, ~subst=emptySubst->substAdd(0, Lam({name: "x", body: Var({idx: 0})})))
  })
  t->block("?0 x y", t => {
    let x = "(x. y. ?0 x y)"
    let y = "(x. y. y x)"
    // ?0 = (x. y. \0 \1)
    t->testUnify(
      x,
      y,
      ~reduce=false,
      ~subst=emptySubst->substAdd(
        0,
        Lam({
          name: "x",
          body: Lam({name: "x", body: App({func: Var({idx: 0}), arg: Var({idx: 1})})}),
        }),
      ),
    )
  })
  t->block("occurs-check (flex-rigid)", t => {
    let a = "(x. ?0 x)"
    let b = "(x. f (?0 x))"
    // ?0 occurs in the rigid term on the right → should not unify
    t->Util.testUnifyFail(a, b)
  })
  t->block("no capture", t => {
    let a = "(x. ?0)"
    let b = "(x. x)"
    // Should fail: it cannot capture the bound variable.
    t->Util.testUnifyFail(a, b)
  })
  t->block("eta", t => {
    t->testUnify(
      "(x. ?0 x)",
      "a",
      ~reduce=true,
      ~subst=emptySubst->substAdd(0, Symbol({name: "a"})),
    )
  })
  t->block("divergent", t => {
    let divergent = "((x. x x) (x. x x))"
    let a = "((x. ?0 x) (x. x x))"
    // TODO: should it not unify or not?
    t->Util.testUnifyFail(a, divergent)
  })
})
