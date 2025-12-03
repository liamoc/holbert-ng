module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

type piece =
  | String(string)
  | Var({idx: int})
  | Schematic({schematic: int, allowed: array<int>})
type t = array<piece>
type meta = string
type schematic = int
type subst = Map.t<schematic, t>
let mapSubst = Util.mapMapValues
let substEqual = Util.mapEqual
let makeSubst = () => Map.make()
let mergeSubsts = Util.mapUnion

let substitute = (term: t, subst: subst) =>
  Array.flatMap(term, piece => {
    switch piece {
    | Schematic({schematic, _}) =>
      switch Map.get(subst, schematic) {
      | None => [piece]
      | Some(found) => found
      }
    | _ => [piece]
    }
  })
let schematicsCountsIn: t => Belt.Map.Int.t<int> = (term: t) =>
  Array.reduce(term, Belt.Map.Int.empty, (m, p) =>
    switch p {
    | Schematic({schematic, _}) =>
      m->Belt.Map.Int.update(schematic, o =>
        o
        ->Option.map(v => v + 1)
        ->Option.orElse(Some(1))
      )
    | _ => m
    }
  )
let maxSchematicCount = (term: t) => {
  schematicsCountsIn(term)->Belt.Map.Int.maximum->Option.map(Pair.second)->Option.getOr(0)
}
let reduce = t => t
let freeVarsIn = (term: t): Belt.Set.t<int, IntCmp.identity> =>
  Array.map(term, piece => {
    switch piece {
    | Var({idx}) => Belt.Set.make(~id=module(IntCmp))->Belt.Set.add(idx)
    | _ => Belt.Set.make(~id=module(IntCmp))
    }
  })->Array.reduce(Belt.Set.make(~id=module(IntCmp)), (s1, s2) => Belt.Set.union(s1, s2))

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

let uncons = (xs: array<'a>): ('a, array<'a>) => {
  switch xs {
  | [] => Error("expected nonempty array")->Result.getExn
  | _ => (xs[0]->Option.getExn, Array.sliceToEnd(xs, ~start=1))
  }
}

type graphSub = Eps | PieceLitSub(piece) | SchemaSub(int, array<int>)
let unify = (s: array<piece>, t: array<piece>, ~gen as _=?): Seq.t<subst> => {
  let match = (p1: piece, p2: piece) => {
    switch (p1, p2) {
    | (String(na), String(nb)) if na == nb => true
    | (Var({idx: ia}), Var({idx: ib})) if ia == ib => true
    | (_, _) => false
    }
  }
  let rec oneSide = (s, t) => {
    switch (s, t) {
    | ([], []) => [emptySubst]
    | ([], _) => []
    | (_, _) => {
        let (s1, ss) = uncons(s)
        switch s1 {
        | Schematic({schematic, allowed}) =>
          Belt.Array.range(0, Array.length(t))
          ->Array.map(i => {
            let subTerm = Array.slice(t, ~start=0, ~end=i)
            let freeVars = freeVarsIn(subTerm)
            let allowedVars = Belt.Set.fromArray(allowed, ~id=module(IntCmp))
            if Belt.Set.subset(freeVars, allowedVars) {
              let s1 = singletonSubst(schematic, subTerm)
              oneSide(
                substitute(ss, s1),
                Array.sliceToEnd(t, ~start=i)->substitute(s1),
              )->Array.map(s2 => combineSubst(s1, s2))
            } else {
              []
            }
          })
          ->Array.flat
        | _ =>
          switch t {
          | [] => []
          | _ => {
              let (t1, ts) = uncons(t)
              if match(s1, t1) {
                oneSide(ss, ts)
              } else {
                []
              }
            }
          }
        }
      }
    }
  }

  let pigPug = (s, t) => {
    let search = (targetCycles: int): (array<subst>, bool) => {
      let moreSolsMightExist = ref(false)
      // seen is an assoc list
      let rec inner = (s, t, cycle: int, seen: array<((t, t), int)>): array<
        array<(int, graphSub)>,
      > => {
        let (newSeen, thisCycle) = switch seen->Array.findIndexOpt(((e, _)) => e == (s, t)) {
        | Some(i) => {
            let (_, thisCycle) = seen[i]->Option.getExn
            let newSeen = seen->Array.mapWithIndex((e, j) => i == j ? ((s, t), cycle + 1) : e)
            (newSeen, thisCycle)
          }

        | None => (Array.concat([((s, t), 1)], seen), 0)
        }
        let cycle = max(thisCycle, cycle)
        let searchSub = (schematic: int, allowed: array<int>, edge: graphSub): array<
          array<(int, graphSub)>,
        > => {
          let piece = Schematic({schematic, allowed})
          let sub = switch edge {
          | Eps => singletonSubst(schematic, [])
          | PieceLitSub(p) => singletonSubst(schematic, [p, piece])
          | SchemaSub(s2, a2) =>
            singletonSubst(schematic, [Schematic({schematic: s2, allowed: a2}), piece])
          }
          inner(substitute(s, sub), substitute(t, sub), cycle, newSeen)->Array.map(path =>
            Array.concat(path, [(schematic, edge)])
          )
        }
        if cycle > targetCycles {
          moreSolsMightExist := true
          []
        } else {
          switch (s[0], t[0]) {
          | (None, None) => cycle == targetCycles ? [[]] : []
          | (Some(Schematic({schematic, allowed})), other)
          | (other, Some(Schematic({schematic, allowed}))) =>
            switch other {
            | None => searchSub(schematic, allowed, Eps)
            | Some(p) =>
              switch p {
              | String(_) =>
                Array.concat(
                  searchSub(schematic, allowed, PieceLitSub(p)),
                  searchSub(schematic, allowed, Eps),
                )
              | Schematic({schematic: s2, allowed: a2}) =>
                if schematic == s2 {
                  inner(
                    s->Array.sliceToEnd(~start=1),
                    t->Array.sliceToEnd(~start=1),
                    cycle,
                    newSeen,
                  )
                } else {
                  Array.concat(
                    searchSub(schematic, allowed, Eps),
                    searchSub(schematic, allowed, SchemaSub(s2, a2)),
                  )
                }
              | Var({idx}) =>
                if Belt.Set.Int.fromArray(allowed)->Belt.Set.Int.has(idx) {
                  Array.concat(
                    searchSub(schematic, allowed, PieceLitSub(p)),
                    searchSub(schematic, allowed, Eps),
                  )
                } else {
                  searchSub(schematic, allowed, Eps)
                }
              }
            }
          | (p1, p2) if p1 == p2 =>
            inner(s->Array.sliceToEnd(~start=1), t->Array.sliceToEnd(~start=1), cycle, newSeen)
          | _ => []
          }
        }
      }
      let paths = inner(s, t, 0, [])
      let substs = paths->Array.map(path => {
        let sub = Map.make()
        path->Array.forEach(((schem, edge)) => {
          Map.set(
            sub,
            schem,
            switch edge {
            | Eps => []
            | PieceLitSub(p) => Array.concat(Map.get(sub, schem)->Option.getOr([]), [p])
            | SchemaSub(s2, _) =>
              Array.concat(
                Map.get(sub, schem)->Option.getOr([]),
                Map.get(sub, s2)->Option.getOr([]),
              )
            },
          )
        })
        sub
      })
      let substsSorted = substs->Array.toSorted((s1, s2) => {
        let substLength = s =>
          s
          ->Util.mapMapValues(Array.length)
          ->Map.values
          ->Iterator.toArray
          ->Array.reduce(0, (acc, v) => acc + v)
        let (s1Length, s2Length) = (substLength(s1), substLength(s2))
        s1Length < s2Length
          ? Ordering.less
          : s2Length < s1Length
          ? Ordering.greater
          : Ordering.equal
      })
      (substsSorted, moreSolsMightExist.contents)
    }
    Seq.unfold((0, true), ((c, moreSolsMightExist)) => {
      if moreSolsMightExist {
        let (substs, moreSolsMightExist) = search(c)
        Some(substs->Seq.fromArray, (c + 1, moreSolsMightExist))
      } else {
        None
      }
    })->Seq.flatten
  }

  // naive: assume schematics appear in at most one side
  let maxCountS = maxSchematicCount(s)
  let maxCountT = maxSchematicCount(t)
  if maxCountS == 0 {
    Seq.fromArray(oneSide(t, s))
  } else if maxCountT == 0 {
    Seq.fromArray(oneSide(s, t))
  } else if max(maxCountS, maxCountT) <= 2 {
    pigPug(s, t)
  } else {
    Seq.fromArray([])
  }
}

// law: unify(a,b) == [{}] iff equivalent(a,b)
let equivalent: (t, t) => bool = (s, t) => s == t
let substDeBruijn = (string: t, substs: array<t>, ~from: int=0) => {
  Array.flatMap(string, piece =>
    switch piece {
    | String(_) => [piece]
    | Var({idx: var}) =>
      if var < from {
        [piece]
      } else if var - from < Array.length(substs) && var - from >= 0 {
        Option.getUnsafe(substs[var - from])
      } else {
        [Var({idx: var - Array.length(substs)})]
      }
    | Schematic({schematic, allowed}) => [
        Schematic({
          schematic,
          allowed: Array.filterMap(allowed, i =>
            if i < from + Array.length(substs) {
              None
            } else {
              Some(i - (from + Array.length(substs)))
            }
          ),
        }),
      ]
    }
  )
}

let upshift = (term: t, amount: int, ~from: int=0) =>
  Array.map(term, piece => {
    switch piece {
    | String(_) => piece
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
  })

let place = (x: int, ~scope: array<string>) => [
  Schematic({
    schematic: x,
    allowed: Array.fromInitializer(~length=Array.length(scope), i => i),
  }),
]

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
let makeGen = () => {
  ref(0)
}

let parseMeta = (str: string) => {
  let re = /^([^\s.\[\]()]+)\./y
  switch re->RegExp.exec(str->String.trim) {
  | None => Error("not a meta name")
  | Some(res) =>
    switch RegExp.Result.matches(res) {
    | [n] => Ok(n, String.sliceToEnd(str->String.trim, ~start=RegExp.lastIndex(re)))
    | _ => Error("impossible happened")
    }
  }
}
let prettyPrintVar = (idx: int, scope: array<string>) =>
  "$" ++
  switch scope[idx] {
  | Some(n) if Array.indexOf(scope, n) == idx => n
  | _ => "\\"->String.concat(String.make(idx))
  }
let prettyPrint = (term: t, ~scope: array<string>) =>
  `"${Array.map(term, piece => {
      switch piece {
      | String(str) => str
      | Var({idx}) => prettyPrintVar(idx, scope)
      | Schematic({schematic, allowed}) => {
          let allowedStr =
            allowed
            ->Array.map(idx => prettyPrintVar(idx, scope))
            ->Array.join(" ")
          `?${Int.toString(schematic)}(${allowedStr})`
        }
      }
    })->Array.join(" ")}"`
let prettyPrintMeta = (str: string) => `${str}.`
let prettyPrintSubst = (sub, ~scope) => Util.prettyPrintMap(sub, ~showV=t => prettyPrint(t, ~scope))

type remaining = string
type errorMessage = string
type ident = string
let parse: (string, ~scope: array<meta>, ~gen: gen=?) => result<(t, remaining), errorMessage> = (
  str: string,
  ~scope: array<ident>,
  ~gen as _=?,
) => {
  let pos = ref(0)
  let seenCloseString = ref(false)
  let acc = ref(Ok([]))

  let error = (msg: errorMessage) => {
    let codeAroundLoc = String.slice(str, ~start=pos.contents, ~end=pos.contents + 5)
    acc := Error(`problem here: ${codeAroundLoc}...: ${msg}`)
  }

  let execRe = Util.execRe
  let advance = n => {
    pos := pos.contents + n
  }
  let advance1 = () => advance(1)
  let add = (token, ~nAdvance=?) => {
    acc.contents
    ->Result.map(acc => {
      Array.push(acc, token)
    })
    ->ignore
    Option.map(nAdvance, advance)->ignore
  }
  let execRe = re => execRe(re, String.sliceToEnd(str, ~start=pos.contents))
  let stringLit = () => {
    let identRegex = RegExp.fromString(`^${Util.identRegexStr}`)
    let symbolRegex = /^([!@#\$%\^~&*_+\-={};':|,.<>\/?]+)/
    let numberRegex = /^(\d+)/
    switch execRe(identRegex)
    ->Option.orElse(execRe(symbolRegex))
    ->Option.orElse(execRe(numberRegex)) {
    | Some([match], l) => add(String(match), ~nAdvance=l)
    | Some(_) => error("regex string lit error")
    | None => error("expected string")
    }
  }
  let escaped = () => {
    let escapedRegex = /\\([\$\?\\\"])/
    switch execRe(escapedRegex) {
    | Some([char], l) => add(String(char), ~nAdvance=l)
    | Some(_) => error("regex escaped error")
    | None => error("expected valid escaped character")
    }
  }
  let readInt = s => Int.fromString(s)->Option.getExn
  let schema = () => {
    let schemaRegex = /\?(\d+)\(((?:\d+\s*)*)\)/
    switch execRe(schemaRegex) {
    | Some([idStr, allowedStr], l) => {
        let schematic = readInt(idStr)
        let allowed =
          allowedStr
          ->String.trim
          ->String.splitByRegExp(/\s+/)
          ->Array.keepSome
          ->Array.filter(s => s != "")
          ->Array.map(readInt)
        add(Schematic({schematic, allowed}), ~nAdvance=l)
      }
    | Some(_) => error("schema lit regex error")
    | None => error("expected schematic literal")
    }
  }
  let var = () => {
    let varLitRegex = /^\$\\(\d+)/
    let varScopeRegex = /^\$([a-zA-Z]\w*)/
    switch execRe(varLitRegex) {
    | Some([match], l) => add(Var({idx: readInt(match)}), ~nAdvance=l)
    | Some(_) => error("var lit regex error")
    | None =>
      switch execRe(varScopeRegex) {
      | Some([ident], l) =>
        switch Array.indexOfOpt(scope, ident) {
        | Some(idx) => add(Var({idx: idx}), ~nAdvance=l)
        | None => error("expected variable in scope")
        }
      | Some(_) => error("var regex error")
      | None => error("expected var")
      }
    }
  }

  // consume leading whitespace + open quote
  switch execRe(/^\s*"/) {
  | Some(_, l) => pos := l
  | None => error("expected open quote")
  }
  while (
    pos.contents < String.length(str) && Result.isOk(acc.contents) && !seenCloseString.contents
  ) {
    let c = String.get(str, pos.contents)->Option.getExn
    switch c {
    | "\"" => {
        advance1()
        seenCloseString := true
      }
    | "$" => var()
    | "?" => schema()
    | " " | "\t" | "\r" | "\n" => advance1()
    | ")" | "(" | "[" | "]" => add(String(c), ~nAdvance=1)
    | "\\" => escaped()
    | _ => stringLit()
    }
  }

  acc.contents->Result.map(r => (r, str->String.sliceToEnd(~start=pos.contents)))
}

let toSExp = t => {
  let convertPiece = p =>
    switch p {
    | String(s) => SExp.Symbol({name: s})
    | Var({idx}) => SExp.Var({idx: idx})
    | Schematic({schematic, allowed}) => SExp.Schematic({schematic, allowed})
    }
  switch Array.length(t) {
  | 1 => convertPiece(t[0]->Option.getExn)
  | _ => SExp.Compound({subexps: Array.map(t, convertPiece)})
  }
}

let rec fromSExp = (t: SExp.t) =>
  switch t {
  | SExp.Symbol({name}) => [String(name)]
  | SExp.Schematic({schematic, allowed}) => [Schematic({schematic, allowed})]
  | SExp.Var({idx}) => [Var({idx: idx})]
  | SExp.Compound({subexps}) => subexps->Array.flatMap(fromSExp)
  }
