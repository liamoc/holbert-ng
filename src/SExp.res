module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

type rec t =
  | Symbol({name: string})
  | Compound({subexps: array<t>})
  | Var({idx: int})
  | Schematic({schematic: int, allowed: array<int>})
type meta = string
type schematic = int
type subst = Map.t<schematic, t>
let mapSubst = (m: subst, f: t => t): subst => {
  let nu = Map.make()
  m->Map.forEachWithKey((v, k) => {
    nu->Map.set(k, f(v))
  })
  nu
}
let makeSubst = () => {
  Map.make()
}
let equivalent = (a: t, b: t) => {
  a == b
}
let reduce = (term: t) => term
let rec schematicsIn: t => Belt.Set.t<int, IntCmp.identity> = (it: t) =>
  switch it {
  | Schematic({schematic, _}) => Belt.Set.make(~id=module(IntCmp))->Belt.Set.add(schematic)
  | Compound({subexps}) =>
    subexps->Array.reduce(Belt.Set.make(~id=module(IntCmp)), (s, x) =>
      Belt.Set.union(s, schematicsIn(x))
    )
  | _ => Belt.Set.make(~id=module(IntCmp))
  }
let rec freeVarsIn: t => Belt.Set.t<int, IntCmp.identity> = (it: t) =>
  switch it {
  | Var({idx}) => Belt.Set.make(~id=module(IntCmp))->Belt.Set.add(idx)
  | Compound({subexps}) =>
    subexps->Array.reduce(Belt.Set.make(~id=module(IntCmp)), (s, x) =>
      Belt.Set.union(s, freeVarsIn(x))
    )
  | _ => Belt.Set.make(~id=module(IntCmp))
  }
let rec substitute = (term: t, subst: subst) =>
  switch term {
  | Compound({subexps}) => Compound({subexps: Array.map(subexps, x => substitute(x, subst))})
  | Schematic({schematic, _}) =>
    switch Map.get(subst, schematic) {
    | None => term
    | Some(found) => found
    }
  | _ => term
  }

let combineSubst = (s: subst, t: subst) => {
  let nu = Map.make()
  Map.entries(s)->Iterator.forEach(opt =>
    switch opt {
    | None => ()
    | Some((key, term)) => nu->Map.set(key, term->substitute(t))
    }
  )
  Map.entries(t)->Iterator.forEach(opt =>
    switch opt {
    | None => ()
    | Some((key, term)) => nu->Map.set(key, term->substitute(s))
    }
  )
  nu
}
let emptySubst: subst = Map.make()
let singletonSubst: (int, t) => subst = (schematic, term) => {
  let s = Map.make()
  s->Map.set(schematic, term)
  s
}
let rec unifyTerm = (a: t, b: t) =>
  switch (a, b) {
  | (Symbol({name: na}), Symbol({name: nb})) if na == nb => Some(emptySubst)
  | (Var({idx: ia}), Var({idx: ib})) if ia == ib => Some(emptySubst)
  | (Schematic({schematic: sa, _}), Schematic({schematic: sb, _})) if sa == sb => Some(emptySubst)
  | (Compound({subexps: xa}), Compound({subexps: xb})) if Array.length(xa) == Array.length(xb) =>
    unifyArray(Belt.Array.zip(xa, xb))
  | (Schematic({schematic, allowed}), t)
    if !Belt.Set.has(schematicsIn(t), schematic) &&
    Belt.Set.subset(freeVarsIn(t), Belt.Set.fromArray(allowed, ~id=module(IntCmp))) =>
    Some(singletonSubst(schematic, t))
  | (t, Schematic({schematic, allowed}))
    if !Belt.Set.has(schematicsIn(t), schematic) &&
    Belt.Set.subset(freeVarsIn(t), Belt.Set.fromArray(allowed, ~id=module(IntCmp))) =>
    Some(singletonSubst(schematic, t))
  | (_, _) => None
  }
and unifyArray = (a: array<(t, t)>) => {
  if Array.length(a) == 0 {
    Some(emptySubst)
  } else {
    let (x, y) = a[0]->Option.getUnsafe
    switch unifyTerm(x, y) {
    | None => None
    | Some(s1) =>
      switch a
      ->Array.sliceToEnd(~start=1)
      ->Array.map(((t1, t2)) => (substitute(t1, s1), substitute(t2, s1)))
      ->unifyArray {
      | None => None
      | Some(s2) => Some(combineSubst(s1, s2))
      }
    }
  }
}
let unify = (a: t, b: t, ~gen as _=?) => {
  switch unifyTerm(a, b) {
  | None => []
  | Some(s) => [s]
  }
}
let rec substDeBruijn = (term: t, substs: array<t>, ~from: int=0) =>
  switch term {
  | Symbol(_) => term
  | Compound({subexps}) =>
    Compound({subexps: Array.map(subexps, x => substDeBruijn(x, substs, ~from))})
  | Var({idx: var}) =>
    if var < from {
      term
    } else if var - from < Array.length(substs) && var - from >= 0 {
      Option.getUnsafe(substs[var - from])
    } else {
      Var({idx: var - Array.length(substs)})
    }
  | Schematic({schematic, allowed}) =>
    Schematic({
      schematic,
      allowed: Array.filterMap(allowed, i =>
        if i < from + Array.length(substs) {
          None
        } else {
          Some(i - (from + Array.length(substs)))
        }
      ),
    })
  }
let rec upshift = (term: t, amount: int, ~from: int=0) =>
  switch term {
  | Symbol(_) => term
  | Compound({subexps}) => Compound({subexps: Array.map(subexps, x => upshift(x, amount, ~from))})
  | Var({idx}) =>
    Var({
      idx: if idx >= from {
        idx + amount
      } else {
        idx
      },
    })
  | Schematic({schematic, allowed}) =>
    Schematic({
      schematic,
      allowed: Array.map(allowed, i =>
        if i >= from {
          i + amount
        } else {
          i
        }
      ),
    })
  }
let place = (x: int, ~scope: array<string>) => Schematic({
  schematic: x,
  allowed: Array.fromInitializer(~length=Array.length(scope), i => i),
})
type gen = ref<int>
let seen = (g: gen, s: int) => {
  if s >= g.contents {
    g := s + 1
  }
}
let fresh = (g: gen, ~replacing as _=?) => {
  let v = g.contents
  g := g.contents + 1
  v
}
let prettyPrintVar = (idx: int, scope: array<string>) =>
  switch scope[idx] {
  | Some(n) if Array.indexOf(scope, n) == idx => n
  | _ => "\\"->String.concat(String.make(idx))
  }
let makeGen = () => {
  ref(0)
}
let rec prettyPrint = (it: t, ~scope: array<string>) =>
  switch it {
  | Symbol({name}) => name
  | Var({idx}) => prettyPrintVar(idx, scope)
  | Schematic({schematic, allowed}) =>
    "?"
    ->String.concat(String.make(schematic))
    ->String.concat("(")
    ->String.concat(Array.join(allowed->Array.map(idx => prettyPrintVar(idx, scope)), " "))
    ->String.concat(")")
  | Compound({subexps}) =>
    "("
    ->String.concat(Array.join(subexps->Array.map(e => prettyPrint(e, ~scope)), " "))
    ->String.concat(")")
  }
let nameRES = "^([^\\s.\\[\\]()]+)\\."
let prettyPrintMeta = (str: string) => {
  String.concat(str, ".")
}
let parseMeta = (str: string) => {
  let re = RegExp.fromStringWithFlags(nameRES, ~flags="y")
  switch re->RegExp.exec(str->String.trim) {
  | None => Error("not a meta name")
  | Some(res) =>
    switch RegExp.Result.matches(res) {
    | [n] => Ok(n, String.sliceToEnd(str->String.trim, ~start=RegExp.lastIndex(re)))
    | _ => Error("impossible happened")
    }
  }
}
let mkParser = (~scope: array<string>, ~gen=?): Parser.t<t> => {
  open Parser
  let inner = fix(f => {
    let varLit = string("\\")->then(decimal)->lexeme
    let ident = regex1(%re(`/([^\s\(\)]+)/`))->lexeme
    let var = varLit->or(
      ident->bind(id =>
        switch scope->Array.indexOfOpt(id) {
        | Some(idx) => pure(idx)
        | None => fail("expected variable")
        }
      ),
    )
    let schemaLit =
      string("?")
      ->then(decimal)
      ->bind(schematic =>
        many(var)
        ->between(token("("), token(")"))
        ->map(
          allowed => {
            gen->Option.map(g => allowed->Array.forEach(n => seen(g, n)))->ignore
            Schematic({schematic, allowed})
          },
        )
      )
    let symbolOrVar = ident->map(symb =>
      switch scope->Array.indexOfOpt(symb) {
      | Some(idx) => Var({idx: idx})
      | None => Symbol({name: symb})
      }
    )
    choice([
      varLit->map(idx => Var({idx: idx})),
      schemaLit,
      symbolOrVar,
      many(f)
      ->between(token("("), token(")"))
      ->map(subexps => Compound({subexps: subexps})),
    ])
  })
  whitespace->then(inner)
}

let parse = (str: string, ~scope: array<string>, ~gen=?) => {
  Parser.runParser(mkParser(~scope, ~gen?), str)->Result.mapError(e => e.message)
}
