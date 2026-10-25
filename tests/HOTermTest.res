open Zora

let parse = (input: string, ~gen=HOTerm.makeGen()) =>
  HOTerm.parse(input, ~grammar=HOTerm.emptyGrammar, ~scope=[], ~gen)
  ->Result.getExn
  ->Pair.first

let renderSubst = (subst: HOTerm.subst) =>
  subst
  ->Belt.Map.Int.toArray
  ->Array.map(((n, t)) =>
    `?${Int.toString(n)} := ${HOTerm.prettyPrint(t, ~grammar=HOTerm.emptyGrammar, ~scope=[])}`
  )
  ->Array.join(", ")

let substOf = (pairs: array<(int, string)>) =>
  Belt.Map.Int.fromArray(pairs->Array.map(((n, str)) => (n, parse(str))))

let checkSound = (t: Zora.t, a: HOTerm.t, b: HOTerm.t, subst: HOTerm.subst) => {
  let lhs = HOTerm.substitute(a, subst)
  let rhs = HOTerm.substitute(b, subst)
  t->ok(
    HOTerm.equivalent(lhs, rhs),
    ~msg=`unifier {${renderSubst(subst)}} makes both sides equivalent: ${HOTerm.prettyPrint(
        lhs,
        ~grammar=HOTerm.emptyGrammar,
        ~scope=[],
      )} vs ${HOTerm.prettyPrint(rhs, ~grammar=HOTerm.emptyGrammar, ~scope=[])}`,
  )
}

let testUnifyOneway = (t: Zora.t, a: string, b: string, expected: array<(int, string)>) => {
  let gen = HOTerm.makeGen()
  let ta = parse(a, ~gen)
  let tb = parse(b, ~gen)
  let solutions = HOTerm.unify(ta, tb, ~gen)->Seq.toArray
  t->equal(solutions->Array.length, 1, ~msg=`assume one unifier for ${a} =?= ${b}`)
  let got = solutions->Array.getUnsafe(0)
  let e = substOf(expected)
  t->ok(
    HOTerm.substEqual(got, e),
    ~msg=`${a} =?= ${b}: expected ${renderSubst(e)}, got ${renderSubst(got)}`,
  )
}

let testUnifyFailOneway = (t: Zora.t, a: string, b: string) => {
  let gen = HOTerm.makeGen()
  let ta = parse(a, ~gen)
  let tb = parse(b, ~gen)
  let solutions = HOTerm.unify(ta, tb, ~gen)->Seq.toArray
  t->equal(solutions->Array.length, 0, ~msg=`assume zero unifier for ${a} =?= ${b}`)
}

let testUnifyFail = (t: Zora.t, a: string, b: string) => {
  t->testUnifyFailOneway(a, b)
  t->testUnifyFailOneway(b, a)
}

let testUnify = (t: Zora.t, a, b, expected) => {
  t->testUnifyOneway(a, b, expected)
  t->testUnifyOneway(b, a, expected)
}

zoraBlock("HOTerm unification", t => {
  t->block("rigid terms", t => {
    t->testUnify("A", "A", [])
    t->testUnify("(f A B)", "(f A B)", [])
    t->testUnifyFail("A", "B")
    t->testUnifyFail("(f A)", "(g A)")
    t->testUnifyFail("(f A)", "(f B)")
    t->testUnifyFail("(f A)", "(f A B)")
    t->testUnifyFail("(@c A)", "(c A)")
    t->testUnify("(@cons A B)", "(@cons ?0 ?1)", [(0, "A"), (1, "B")])
    t->testUnify("(f (g A) B)", "(f (g A) B)", [])
  })

  t->block("beta and eta", t => {
    t->testUnify("((x. f x) A)", "(f A)", [])
    t->testUnify("((x. (y. f x y)) A B)", "(f A B)", [])
    t->testUnify("(x. f x)", "(y. f y)", [])
    t->testUnify("(x. f x)", "f", [])
    t->testUnify("(x. ?0 x)", "?0", [])
    t->testUnify("(x. (y. g x y))", "(x. g x)", [])
  })

  t->block("flex-rigid (first order)", t => {
    t->testUnify("?0", "A", [(0, "A")])
    t->testUnify("(f ?0)", "(f A)", [(0, "A")])
    t->testUnify("(f ?0 ?1)", "(f A B)", [(0, "A"), (1, "B")])
    t->testUnify("(f ?0 B)", "(f A ?1)", [(0, "A"), (1, "B")])
    t->testUnifyFail("(f ?0 A)", "(f B C)")
    t->testUnify("?0", "(f A B)", [(0, "(f A B)")])
    t->testUnify("(Implies A B)", "(Implies ?0 ?1)", [(0, "A"), (1, "B")])
    t->testUnify("(f ?0 ?0)", "(f A A)", [(0, "A")])
    t->testUnifyFail("(f ?0 ?0)", "(f A B)")
    t->testUnifyOneway("(f ?0 ?1)", "(f ?1 ?0)", [(0, "?1")])
  })

  t->block("occurs check", t => {
    t->testUnifyFail("?0", "(f ?0)")
    t->testUnifyFail("(f ?0)", "?0")
    t->testUnifyFail("?0", "(f (g ?0))")
    t->testUnifyFail("(x. ?0)", "(x. f ?0)")
    t->testUnifyFail("(x. ?0 x)", "(x. ?0 (?0 x))")
    t->testUnify("(x. ?0 x)", "(x. f (?1 x))", [(0, "(x. f (?1 \\0))")])
  })

  t->block("pattern unification", t => {
    t->testUnify("(x. ?0 x)", "(x. f x)", [(0, "(x. f \\0)")])
    t->testUnify("(x. ?0 x)", "(x. x)", [(0, "(x. \\0)")])
    t->testUnify("(x. (y. ?0 x y))", "(x. (y. f x y))", [(0, "(x. (y. f \\1 \\0))")])
    t->testUnify("(x. (y. ?0 y x))", "(x. (y. f x y))", [(0, "(x. (y. f \\0 \\1))")])
    t->testUnify("(x. (y. ?0 x y))", "(x. (y. f x))", [(0, "(x. (y. f \\1))")])
    t->testUnify("(x. (y. (z. ?0 y z)))", "(x. (y. (z. f y z)))", [(0, "(x. (y. f \\1 \\0))")])
    t->testUnify("(x. ?0 x x)", "(x. f x)", [(0, "(x. (y. f \\0))")])
    t->testUnifyFail("(x. (y. ?0 x))", "(x. (y. f y))")
    t->testUnifyFail("(x. (y. ?0 y))", "(x. (y. f x))")
    t->testUnifyFail("(x. ?0)", "(x. \\0)")
    t->testUnify("(x. ?0)", "(x. A)", [(0, "A")])
    t->testUnify("(x. ?0 x)", "(x. (y. f x y))", [(0, "(x. (y. f \\1 \\0))")])
  })

  t->block("non-pattern", t => {
    t->testUnifyFail("(x. ?0 c)", "(x. h c)")
    t->testUnifyFail("(x. ?0 (y. f x y))", "(x. h)")
    t->testUnifyFail("(x. ?0 (?1 x))", "(x. h x)")
    t->testUnifyFail("(x. ?0 (?1 x))", "(x. ?0 (?2 x))")
    t->testUnifyFail("(x. ?0 (f x))", "(x. g (f x))")
    t->testUnifyFail("(x. ?0 (f x))", "(x. g x)")
    t->testUnifyFail("(x. (y. ?0 (f x) (g y)))", "(x. (y. h x))")
    t->testUnifyFail("(x. ?0 (f x))", "(x. x)")
    t->testUnifyFail("(x. ?0 (f x))", "(x. h (?0 (f x)))")
    t->testUnifyFail("(x. ?0 (f x))", "(x. ?1 (g x))")
    t->testUnifyFail("(x. ?0 x)", "(x. ?0 (?0 x))")
  })

  t->block("flex-flex-same", t => {
    t->testUnify("(x. ?0 x)", "(x. ?0 x)", [])
    t->testUnify("(x. ?0 (f x))", "(x. ?0 (f x))", [])
    t->testUnify("(x. (y. ?0 x y))", "(x. (y. ?0 y x))", [(0, "(x. (y. ?1))")])
    t->testUnify("(x. (y. (z. ?0 x y)))", "(x. (y. (z. ?0 x z)))", [(0, "(x. (y. ?1 \\1))")])
    t->testUnify("(x. (y. (z. ?0 x y)))", "(x. (y. (z. ?0 z y)))", [(0, "(x. (y. ?1 \\0))")])
    t->testUnifyFail("(x. ?0 x)", "(x. ?0 x x)")
  })

  t->block("flex-flex-diff", t => {
    t->testUnifyOneway("(x. ?0 x)", "(x. ?1 x)", [(0, "(x. ?1 \\0)")])
    t->testUnifyOneway("(x. (y. ?0 x y))", "(x. (y. ?1 y x))", [(0, "(x. (y. ?1 \\0 \\1))")])
    t->testUnifyOneway("?0", "?1", [(0, "?1")])
    t->testUnify("(x. ?0 x)", "(x. f (?1 x))", [(0, "(x. f (?1 \\0))")])
    t->testUnifyFail("(x. (y. ?0 x))", "(x. (y. ?1 y))")
  })

  t->block("de Bruijn bookkeeping", t => {
    t->testUnify("(x. (y. ?0 x y))", "(x. (y. f y))", [(0, "(x. (y. f \\0))")])
    t->testUnify("(x. (y. (z. ?0 x z)))", "(x. (y. (z. g z x)))", [(0, "(x. (y. g \\0 \\1))")])
    t->testUnifyOneway("(x. ?0 x)", "(x. (y. ?1 x y))", [(0, "(x. (y. ?1 \\1 \\0))")])
    t->testUnify("(x. (y. ?0 x y))", "(x. (y. ?1 (?2 y) x))", [(0, "(x. (y. ?1 (?2 \\0) \\1))")])
  })

  t->block("equivalent", t => {
    t->testUnify("A", "A", [])
    t->testUnify("(@c A)", "(@c A)", [])
    t->testUnify("(f A (g B))", "(f A (g B))", [])
    t->testUnify("(x. f x)", "f", [])
    t->testUnify("((x. f x) A)", "(f A)", [])
    t->testUnify("(x. ?0 x)", "?0", [])
  })

  t->block("symmetry", t => {
    t->testUnify("(x. ?0 x)", "(x. f x)", [(0, "(x. f \\0)")])
    t->testUnifyOneway("?0", "?1", [(0, "?1")])
    t->testUnifyOneway("?1", "?0", [(1, "?0")])
    t->testUnifyFail("(x. ?0 (f x))", "(x. g x)")
    t->testUnifyFail("(x. (y. ?0 x))", "(x. (y. f y))")
  })
})
