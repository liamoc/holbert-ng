module type SYMBOL = {
  type t
  type subst = Map.t<int, t>
  let unify: (t, t) => Seq.t<subst>
  let prettyPrint: (t, ~scope: array<string>) => string
  let parse: (string, ~scope: array<string>, ~gen: ref<int>=?) => result<(t, string), string>
  let substitute: (t, subst) => t
  // used for when trying to substitute a variable of the wrong type
  let lowerVar: int => t
  let lowerSchematic: (int, array<int>) => t
  let ghost: t
  let substDeBruijn: (t, array<t>, ~from: int) => t
  // used for grouping judgments together for rule induction
  let constSymbol: t => option<string>
}

module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

module Make = (Symbol: SYMBOL): {
  type rec t =
    | Symbol(Symbol.t)
    | Compound({subexps: array<t>})
    | Var({idx: int})
    | Schematic({schematic: int, allowed: array<int>})
    | Ghost
  module Symbol: SYMBOL with type t := Symbol.t
  include Signatures.TERM
    with type t := t
    and type meta = string
    and type schematic = int
    and type subst = Map.t<int, t>
} => {
  type rec t =
    | Symbol(Symbol.t)
    | Compound({subexps: array<t>})
    | Var({idx: int})
    | Schematic({schematic: int, allowed: array<int>})
    | Ghost
  module Symbol = Symbol
  type meta = string
  type schematic = int
  type subst = Map.t<schematic, t>
  let substEqual = Util.mapEqual
  let mapSubst = Util.mapMapValues
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
    | Symbol(name) => {
        let symbolSubs =
          subst
          ->Map.entries
          ->Iterator.toArray
          ->Array.filterMap(((name, v)) =>
            switch v {
            | Symbol(v) => Some((name, v))
            | _ => None
            }
          )
          ->Map.fromArray
        Symbol(name->Symbol.substitute(symbolSubs))
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
  let rec unifyTerm = (a: t, b: t): Seq.t<subst> =>
    switch (a, b) {
    | (Symbol(na), Symbol(nb)) =>
      Symbol.unify(na, nb)->Seq.map(subst => subst->Util.mapMapValues(v => Symbol(v)))
    | (Compound({subexps: xa}), Compound({subexps: xb})) if Array.length(xa) == Array.length(xb) =>
      unifyArray(Belt.Array.zip(xa, xb))
    | (Var({idx: ia}), Var({idx: ib})) if ia == ib => Seq.once(emptySubst)
    | (Schematic({schematic: sa, _}), Schematic({schematic: sb, _})) if sa == sb =>
      Seq.once(emptySubst)
    | (Schematic({schematic, allowed}), t)
      if !Belt.Set.has(schematicsIn(t), schematic) &&
      Belt.Set.subset(freeVarsIn(t), Belt.Set.fromArray(allowed, ~id=module(IntCmp))) =>
      Seq.once(singletonSubst(schematic, t))
    | (t, Schematic({schematic, allowed}))
      if !Belt.Set.has(schematicsIn(t), schematic) &&
      Belt.Set.subset(freeVarsIn(t), Belt.Set.fromArray(allowed, ~id=module(IntCmp))) =>
      Seq.once(singletonSubst(schematic, t))
    | (_, _) => Seq.empty
    }
  and unifyArray = (a: array<(t, t)>): Seq.t<subst> => {
    if Array.length(a) == 0 {
      Seq.once(emptySubst)
    } else {
      let (x, y) = a[0]->Option.getUnsafe
      unifyTerm(x, y)->Seq.flatMap(s1 =>
        a
        ->Array.sliceToEnd(~start=1)
        ->Array.map(((t1, t2)) => (substitute(t1, s1), substitute(t2, s1)))
        ->unifyArray
        ->Seq.map(s2 => combineSubst(s1, s2))
      )
    }
  }
  let unify = (a: t, b: t, ~gen as _=?) => unifyTerm(a, b)

  let rec lower = (term: t): Symbol.t =>
    switch term {
    | Symbol(s) => s
    | Var({idx}) => Symbol.lowerVar(idx)
    | Schematic({schematic, allowed}) => Symbol.lowerSchematic(schematic, allowed)
    | Compound({subexps: [e1]}) => lower(e1)
    | _ => Symbol.ghost
    }
  let rec substDeBruijn = (term: t, substs: array<t>, ~from: int=0) =>
    switch term {
    | Symbol(s) => {
        let symbolSubsts = substs->Array.map(lower)
        Symbol(Symbol.substDeBruijn(s, symbolSubsts, ~from))
      }

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
    | Ghost => Ghost
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
    | Ghost => Ghost
    }
  let place = (x: int, ~scope: array<string>) => Schematic({
    schematic: x,
    allowed: Array.fromInitializer(~length=Array.length(scope), i => i),
  })
  let mergeSubsts = Util.mapUnion

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
  let prettyPrintVar = (idx: int, scope: array<string>) => {
    switch scope[idx] {
    | Some(n) if Array.indexOf(scope, n) == idx => n
    | _ => "\\"->String.concat(String.make(idx))
    }
  }
  let makeGen = () => {
    ref(0)
  }
  let rec prettyPrint = (it: t, ~scope: array<string>) =>
    switch it {
    | Symbol(name) => Symbol.prettyPrint(name, ~scope)
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
    | Ghost => "§SExp.Ghost"
    }

  let prettyPrintSubst = (sub, ~scope) =>
    Util.prettyPrintMap(sub, ~showV=t => prettyPrint(t, ~scope))
  let symbolRegexpString = `^([^\\s()\\[\\]]+)`
  let varRegexpString = "^\\\\([0-9]+)$"
  let schematicRegexpString = "^\\?([0-9]+)$"
  type lexeme = LParen | RParen | VarT(int) | SymbolT(Symbol.t) | SchematicT(int)
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
  let parse = (str: string, ~scope: array<string>, ~gen=?) => {
    let cur = ref(String.make(str))
    let lex: unit => option<lexeme> = () => {
      let str = String.trim(cur.contents)
      cur := str
      let checkVariable = (candidate: string) => {
        let varRegexp = RegExp.fromString(varRegexpString)
        switch Array.indexOf(scope, candidate) {
        | -1 =>
          switch varRegexp->RegExp.exec(candidate) {
          | Some(res') =>
            switch RegExp.Result.matches(res') {
            | [idx] => Some(idx->Int.fromString->Option.getUnsafe)
            | _ => None
            }
          | None => None
          }
        | idx => Some(idx)
        }
      }
      if String.get(str, 0) == Some("(") {
        cur := String.sliceToEnd(str, ~start=1)
        Some(LParen)
      } else if String.get(str, 0) == Some(")") {
        cur := String.sliceToEnd(str, ~start=1)
        Some(RParen)
      } else {
        let symbolRegexp = RegExp.fromStringWithFlags(symbolRegexpString, ~flags="y")
        switch symbolRegexp->RegExp.exec(str) {
        | None => None
        | Some(res) =>
          switch RegExp.Result.matches(res) {
          | [symb] => {
              let specialSymb = tok => {
                cur := String.sliceToEnd(str, ~start=RegExp.lastIndex(symbolRegexp))
                Some(tok)
              }
              let regularSymb = () => {
                // FIX: not ideal to throw away symbol error message
                Console.log(("current", cur.contents))
                Symbol.parse(cur.contents, ~scope)
                ->Util.Result.ok
                ->Option.map(((s, rest)) => {
                  cur := rest
                  Console.log(("parsed", s, cur.contents))
                  SymbolT(s)
                })
              }
              switch checkVariable(symb) {
              | Some(idx) => specialSymb(VarT(idx))
              | None => {
                  let schematicRegexp = RegExp.fromString(schematicRegexpString)
                  switch schematicRegexp->RegExp.exec(symb) {
                  | None => regularSymb()
                  | Some(res') =>
                    switch RegExp.Result.matches(res') {
                    | [s] => specialSymb(SchematicT(s->Int.fromString->Option.getUnsafe))
                    | _ => regularSymb()
                    }
                  }
                }
              }
            }
          | _ => None
          }
        }
      }
    }

    let peek = () => {
      // a bit slow, better would be to keep a backlog of lexed tokens..
      let str = String.make(cur.contents)
      let tok = lex()
      cur := str
      tok
    }
    exception ParseError(string)
    let rec parseExp = () => {
      let tok = peek()
      switch tok {
      | Some(SymbolT(s)) => {
          let _ = lex()
          Some(Symbol(s))
        }
      | Some(VarT(idx)) => {
          let _ = lex()
          Some(Var({idx: idx}))
        }
      | Some(SchematicT(num)) => {
          let _ = lex()
          switch lex() {
          | Some(LParen) => {
              let it = ref(None)
              let bits = []
              let getVar = (t: option<lexeme>) =>
                switch t {
                | Some(VarT(idx)) => Some(idx)
                | _ => None
                }
              while {
                it := lex()
                it.contents->getVar->Option.isSome
              } {
                Array.push(bits, it.contents->getVar->Option.getUnsafe)
              }
              switch it.contents {
              | Some(RParen) =>
                switch gen {
                | Some(g) => {
                    seen(g, num)
                    Some(Schematic({schematic: num, allowed: bits}))
                  }
                | None => throw(ParseError("Schematics not allowed here"))
                }
              | _ => throw(ParseError("Expected closing parenthesis"))
              }
            }
          | _ => throw(ParseError("Expected opening parenthesis"))
          }
        }
      | Some(LParen) => {
          let _ = lex()
          let bits = []
          let it = ref(None)
          while {
            it := parseExp()
            it.contents->Option.isSome
          } {
            Array.push(bits, it.contents->Option.getUnsafe)
          }
          switch lex() {
          | Some(RParen) => Some(Compound({subexps: bits}))
          | _ => throw(ParseError("Expected closing parenthesis"))
          }
        }
      | _ => None
      }
    }
    switch parseExp() {
    | exception ParseError(s) => Error(s)
    | None => Error("No expression to parse")
    | Some(e) => Ok((e, cur.contents))
    }
  }

  let ghostTerm = Ghost
}
