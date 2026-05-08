open Zora

module Symbol = AtomDef.MakeAtomChoiceAndView(
  Symbolic.Atom,
  Symbolic.AtomView,
  AtomDef.EmptyAtomChoice,
  AtomDef.EmptyAtomChoiceView,
)
module RegularHOTerm = HOTerm.Make(Symbol.Atom)
module Util = TestUtil.MakeTerm(RegularHOTerm)
module StringSymbol = AtomDef.MakeAtomChoiceAndView(
  StringA.Atom,
  StringA.AtomView,
  Symbol.Atom,
  Symbol.AtomView,
)
module StringHOTerm = HOTerm.Make(StringSymbol.Atom)
module StringUtil = TestUtil.MakeTerm(StringHOTerm)
let wrapString = s => StringHOTerm.Symbol({
  name: AtomBase.AnyValue(StringA.Base.Tag, s),
  constructor: false,
})
let wrapSymbol = s => StringHOTerm.Symbol({
  name: AtomBase.AnyValue(Symbolic.Base.Tag, s),
  constructor: false,
})

let testUnify0 = (t: Zora.t, at: string, bt: string, ~subst=?, ~msg=?, ~reduce=false) => {
  let gen = RegularHOTerm.makeGen()
  let (a, _) = RegularHOTerm.parse(at, ~scope=[], ~gen)->Result.getExn
  let (b, _) = RegularHOTerm.parse(bt, ~scope=[], ~gen)->Result.getExn
  try {
    let res0 =
      RegularHOTerm.unifyTerm(a, b, RegularHOTerm.emptySubst, ~gen=Some(gen))
      ->Seq.head
      ->Option.getExn
    let res = if reduce {
      RegularHOTerm.reduceSubst(res0)
    } else {
      res0
    }
    switch subst {
    | None => t->ok(true, ~msg=msg->Option.getOr("unification succeeded"))
    | Some(subst) =>
      t->equal(subst->Belt.Map.Int.size, res->Belt.Map.Int.size)
      subst->Belt.Map.Int.forEach((k, _v) => {
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
  | RegularHOTerm.UnifyFail(failed) =>
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
let testUnify = (t: Zora.t, at: string, bt: string, ~subst=?, ~msg=?, ~reduce=false) => {
  testUnify0(t, at, bt, ~subst?, ~msg?, ~reduce)
  testUnify0(t, bt, at, ~subst?, ~msg?, ~reduce)
}
let s = s => RegularHOTerm.Symbol({name: Symbolic.Base.wrap(s), constructor: false})
zoraBlock("parse symbol", t => {
  t->block("single char", t => t->Util.testParse("x", s("x")))
  t->block("multi char", t => t->Util.testParse("xyz", s("xyz")))
})

zoraBlock("parse var", t => {
  t->block("single digit", t => t->Util.testParse("\\1", Var({idx: 1})))
  t->block("multi digit", t => t->Util.testParse("\\234", Var({idx: 234})))
  t->block("scope", t => t->Util.testParse("0", ~scope=["0"], Var({idx: 0})))
})

zoraBlock("parse schematic", t => {
  t->block("empty allowed", t => t->Util.testParse("?1", Schematic({schematic: 1})))
})

zoraBlock("parse application", t => {
  t->block("multiple", t => {
    t->Util.testParse(
      "(a b)",
      App({
        func: s("a"),
        arg: s("b"),
      }),
    )
  })
  t->block("multiple more", t => {
    t->Util.testParse(
      "(a b c)",
      App({
        func: App({
          func: s("a"),
          arg: s("b"),
        }),
        arg: s("c"),
      }),
    )
  })
  t->block("multiple var", t => {
    t->Util.testParse(
      "(a \\1 ?1)",
      App({
        func: App({func: s("a"), arg: Var({idx: 1})}),
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
  t->block("constructor", t => {
    t->Util.testParse("@cons", Symbol({name: Symbolic.Base.wrap("cons"), constructor: true}))
  })
  // TODO: test if remaining strings are returned correctly
})

zoraBlock("parse and prettyprint", t => {
  t->block("examples", t => {
    t->Util.testParsePrettyPrint("\\1", "\\1")
    t->Util.testParsePrettyPrint("?1", "?1")
    t->Util.testParsePrettyPrint("@cons", "@cons")
    t->Util.testParsePrettyPrint("(x. x)", "(x. x)")
    t->Util.testParsePrettyPrint("(x. x. \\0)", "(x. x. x)")
    t->Util.testParsePrettyPrint("(x. x. \\1)", "(x. x. \\1)")
    t->Util.testParsePrettyPrint("(x. y. z. z)", "(x. y. z. z)")
    t->Util.testParsePrettyPrint("(x. y. z. y)", "(x. y. z. y)")
    t->Util.testParsePrettyPrint("(x. y. z. x)", "(x. y. z. x)")
    t->Util.testParsePrettyPrint("(x. y. z. z y x)", "(x. y. z. z y x)")
  })
})

zoraBlock("string HOTerm functor", t => {
  module B = AtomBase.String
  t->block("parse string atom", t => {
    t->StringUtil.testParse(`"x y"`, wrapString([String("x"), String("y")]))
    t->StringUtil.testParse(`"$s"`, ~scope=["s"], wrapString([AtomBase.String.Var({idx: 0})]))
    t->StringUtil.testParsePrettyPrint(`"x y"`, `"x y"`)
  })
  t->block("parse symbolic atom", t => {
    t->StringUtil.testParse("x", wrapSymbol("x"))
    t->StringUtil.testParse(
      "@cons",
      StringHOTerm.Symbol({
        name: AtomBase.AnyValue(Symbolic.Base.Tag, "cons"),
        constructor: true,
      }),
    )
    t->StringUtil.testParse(
      "(x. x)",
      StringHOTerm.Lam({name: "x", body: StringHOTerm.Var({idx: 0})}),
    )
  })
  t->block("unify string atom", t => {
    let parse = (input, ~scope=[]) => t->StringUtil.parse(input, ~scope)
    let emptySubst = StringHOTerm.emptySubst
    let substAdd = StringHOTerm.substAdd
    t->equal(
      StringHOTerm.unify(parse(`"a ?0() c"`), parse(`"a b c"`))->Seq.head,
      Some(emptySubst->substAdd(0, wrapString([String("b")]))),
    )
    t->block(
      "ahhhhhhhhhh",
      t => {
        let scope = ["s"]
        let gen = StringHOTerm.makeGen()
        let schemas =
          scope->Array.map(m => StringHOTerm.place(gen->StringHOTerm.fresh(~replacing=m), ~scope))
        let lhs = parse(`("$s")`, ~scope)
        let rhs = parse(`("?0(0)")`)
        let res = StringHOTerm.unify(lhs, rhs)->Seq.head
        t->equal(res, Some(emptySubst->substAdd(0, wrapString([Var({idx: 0})]))))
        let sub = res->Option.getExn
        Console.log(StringHOTerm.prettyPrintSubst(sub, ~scope))
        let deBruijnSub = schemas->Array.map(t => StringHOTerm.substitute(t, sub))
        // fails schemas[0] == (?0(0) \0) => deBruijnSub[0] == ("$0" \0)
        t->equal(deBruijnSub[0], Some(wrapString([Var({idx: 0})])))
        // throws currently due to failure to `lower` ("$0" \0) into string
        let newLhs = StringHOTerm.substDeBruijn(lhs, deBruijnSub)
        t->equal(newLhs, lhs)
      },
    )

    t->equal(
      StringHOTerm.unify(parse(`(P "?1() a" "?1()")`), parse(`(P "a ?1()" "a")`))->Seq.head,
      Some(emptySubst->substAdd(1, wrapString([String("a")]))),
    )
    let choices =
      StringHOTerm.unify(parse(`"?1() a"`), parse(`"a ?1()"`))
      ->Seq.take(2)
      ->Seq.toArray
    t->equal(
      choices,
      [emptySubst->substAdd(1, wrapString([])), emptySubst->substAdd(1, wrapString([String("a")]))],
    )
    t->equal(StringHOTerm.unify(parse(`"a"`), parse(`"b"`))->Seq.head, None)
  })
})

zoraBlock("unify test", t => {
  let testUnifyFail = Util.testUnifyFailString
  open RegularHOTerm
  t->block("symbols", t => {
    let x = "x"
    let y = "y"
    t->testUnify(x, x)
    t->testUnifyFail(y, x)
    t->testUnifyFail(x, y)
  })
  t->block("applications", t => {
    let ab = "(a b)"
    let cd = "(c d)"
    t->testUnify(ab, ab)
    t->testUnify(cd, cd)
    t->testUnifyFail(ab, cd)
    t->testUnifyFail(cd, ab)
  })
  t->block("flex-rigid", t => {
    let x = "?0"
    let y = "y"
    t->testUnify(x, y, ~subst=RegularHOTerm.emptySubst->RegularHOTerm.substAdd(0, s("y")))
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
        Lam({
          name: "x",
          body: App({func: s("y"), arg: Var({idx: 0})}),
        }),
      ),
    )
    t->testUnify(x, y, ~reduce=true, ~subst=emptySubst->substAdd(0, s("y")))
  })
  t->block("flex-rigid3", t => {
    let x = "(?0 \\10)"
    let y = "(fst \\10)"
    t->testUnify(x, y, ~reduce=true, ~subst=emptySubst->substAdd(0, s("fst")))
  })
  t->block("flex-rigid", t => {
    let x = "(?0 \\10)"
    let y = "(r (fst \\10))"
    t->Util.testUnify1(
      x,
      y,
      ~subst=emptySubst->substAdd(
        0,
        Lam({
          name: "x",
          body: App({
            func: s("r"),
            arg: App({func: s("fst"), arg: Var({idx: 0})}),
          }),
        }),
      ),
    )
  })
  t->block("flex-rigid-fcu-2", t => {
    let x = "(?0 (fst \\10))"
    let y = "(r (fst (fst \\10)))"
    t->Util.testUnify1(
      x,
      y,
      ~subst=emptySubst->substAdd(
        0,
        RegularHOTerm.parse("(x. (r (fst x)))", ~scope=[])->Result.getExn->Pair.first,
      ),
    )
  })
  t->block("flex-rigid-fcu-3", t => {
    let x = "(?1 (fst \\10) (snd \\1))"
    let y = "(r (q (snd \\1) (fst \\10)))"
    t->Util.testUnify1(
      x,
      y,
      ~subst=emptySubst->substAdd(
        1,
        RegularHOTerm.parse("(x. x. (r (q \\0 \\1)))", ~scope=[])->Result.getExn->Pair.first,
      ),
    )
  })
  t->block("flex-rigid-fcu-4", t => {
    let x = "(?1 (fst \\10) \\1)"
    let y = "(r (q (snd \\1) (fst \\10)))"
    t->Util.testUnify1(
      x,
      y,
      ~subst=emptySubst->substAdd(
        1,
        RegularHOTerm.parse("(x. x. (r (q (snd \\0) \\1)))", ~scope=[])->Result.getExn->Pair.first,
      ),
    )
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
    t->testUnifyFail(a, b)
  })
  t->block("no capture", t => {
    let a = "(x. ?0)"
    let b = "(x. x)"
    // Should fail: it cannot capture the bound variable.
    t->testUnifyFail(a, b)
  })
  t->block("eta", t => {
    t->testUnify("(x. ?0 x)", "a", ~reduce=true, ~subst=emptySubst->substAdd(0, s("a")))
  })
  t->block("divergent", _t => {
    let _divergent = "((x. x x) (x. x x))"
    let _a = "((x. ?0 x) (x. x x))"
    // we don't care
    // t->Util.testNotUnify(a, divergent)
  })
  t->block("dup var", t => {
    let a = "(?0 \\0 \\0)"
    let b = "\\0"
    t->testUnify(a, b, ~subst=emptySubst->substAdd(0, t->Util.parse("(x. x. \\0)")))
  })
  t->block("dup var 2", t => {
    let a = "(k. ?0 k k)"
    let b = "(k. G k)"
    t->testUnify(a, b, ~subst=emptySubst->substAdd(0, t->Util.parse("(x. x. G x)")))
  })
  // Yokoyama et al.'s example in A Functional Implementation of  Function-as-Constructor Higher-Order Unification  Makoto Hamana1
  t->block("break global resctriction", t => {
    t->testUnify(
      "(x. y. ?0 (c x) (c y))",
      "(x. y. c (?1 x y))",
      ~reduce=true,
      ~subst=emptySubst
      ->substAdd(0, t->Util.parse("(x. x. (c ?2))"))
      ->substAdd(1, t->Util.parse("(x. x. ?2)")),
    )
  })
  t->block("violate global restriction only", t => {
    t->testUnify(
      "(l. (?0 (fst l)))",
      "(l. (snd (?1 (cons (fst l) (snd l)))))",
      ~reduce=true,
      ~subst=emptySubst
      ->substAdd(0, t->Util.parse("(x. (snd ?2))"))
      ->substAdd(1, t->Util.parse("(x. ?2)")),
    )
  })
  t->block("violate local restriction", t => {
    let a = "(?0 (fst l) l)"
    let b = "(cons l)"
    t->testUnify(a, b, ~subst=emptySubst->substAdd(0, t->Util.parse("(x. x. cons x)")))
  })
  t->block("nat tests", t => {
    let a = "(Nat (S ?6))"
    let b = "(Nat (S (S \\0)))"
    let c = "(Nat (S (?6 \\0)))"
    t->testUnifyFail(a, b)
    t->testUnify(c, b, ~subst=emptySubst->substAdd(6, t->Util.parse("(x. S \\0)")))
  })
  t->block("tests from induction examples", _t => {
    let _r = "((?0 \\0) (?1 \\0))"
    let _g = "(f \\0)"
    // what it's currently doing:
    //   0 := (x. y. f x)
    //   1 := doesn't matter
    // what we want
    //   0 := f
    //   1 := (x. x)
    //t->testUnify(r, g, ~subst=emptySubst->substAdd(0, t->Util.parse("(x. f x)"))->substAdd(1,t->Util.parse("(x. x)")))
  })
})
