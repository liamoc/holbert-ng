module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

type rec piece =
  | String(string)
  | Var({idx: int})
  | Schematic({schematic: int, allowed: array<int>})
type t = array<piece>
type meta = string
type schematic = int
type subst = Map.t<schematic, t>

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
let schematicsIn: t => Belt.Set.t<int, IntCmp.identity> = (term: t) =>
  Array.map(term, piece => {
    switch piece {
    | Schematic({schematic, _}) => Belt.Set.make(~id=module(IntCmp))->Belt.Set.add(schematic)
    | _ => Belt.Set.make(~id=module(IntCmp))
    }
  })->Array.reduce(Belt.Set.make(~id=module(IntCmp)), (s1, s2) => Belt.Set.union(s1, s2))
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

let unify = (s: array<piece>, t: array<piece>): array<subst> => {
  let match = (p1: piece, p2: piece) => {
    switch (p1, p2) {
    | (String(na), String(nb)) if na == nb => true
    | (Var({idx: ia}), Var({idx: ib})) if ia == ib => true
    | (_, _) => false
    }
  }
  let rec inner = (s, t) => {
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
              inner(
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
                inner(ss, ts)
              } else {
                []
              }
            }
          }
        }
      }
    }
  }

  // naive: assume schematics appear in at most one side
  if schematicsIn(s)->Belt.Set.isEmpty {
    inner(t, s)
  } else if schematicsIn(t)->Belt.Set.isEmpty {
    inner(s, t)
  } else {
    []
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
  let re = %re("/^([^\s.\[\]()]+)\./y")
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
  switch scope[idx] {
  | Some(n) if Array.indexOf(scope, n) == idx && false => n
  | _ => "\\"->String.concat(String.make(idx))
  }
let prettyPrint = (term: t, ~scope: array<string>) =>
  Array.map(term, piece => {
    switch piece {
    | String(str) => `"${str}"`
    | Var({idx}) => prettyPrintVar(idx, scope)
    | Schematic({schematic, allowed}) => {
        let allowedStr =
          allowed
          ->Array.map(idx => prettyPrintVar(idx, scope))
          ->Array.join(" ")
        `?${Int.toString(schematic)}(${allowedStr})`
      }
    }
  })->Array.join(".")
let prettyPrintMeta = (str: string) => `${str}.`

type remaining = string
type errorMessage = string
type ident = string
let parse: (string, ~scope: array<meta>, ~gen: gen=?) => result<(t, remaining), errorMessage> = (
  str: string,
  ~scope: array<ident>,
  ~gen=?,
) => {
  let pos = ref(0)
  let seenCloseString = ref(false)
  let acc = ref(Ok([]))

  let error = (msg: errorMessage) => {
    let codeAroundLoc = String.slice(str, ~start=pos.contents, ~end=pos.contents + 5)
    acc := Error(`problem here: ${codeAroundLoc}...: ${msg}`)
  }

  let execRe = (re, str) => {
    re
    ->RegExp.exec(str)
    ->Option.map(result => {
      open RegExp.Result
      (matches(result), fullMatch(result)->String.length)
    })
  }
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
    let identRegex = %re(`/^([a-zA-Z][a-zA-Z\d]*)/`)
    let symbolRegex = %re(`/^([!@#\$%\^~&*_+\-={};':|,.<>\/?]+)/`)
    let numberRegex = %re(`/^(\d+)/`)
    switch execRe(identRegex)
    ->Option.orElse(execRe(symbolRegex))
    ->Option.orElse(execRe(numberRegex)) {
    | Some([match], l) => add(String(match), ~nAdvance=l)
    | Some(_) => error("regex string lit error")
    | None => error("expected string")
    }
  }
  let escaped = () => {
    let escapedRegex = %re(`/\\([\$\?\\\"])/`)
    switch execRe(escapedRegex) {
    | Some([char], l) => add(String(char), ~nAdvance=l)
    | Some(_) => error("regex escaped error")
    | None => error("expected valid escaped character")
    }
  }
  let readInt = s => Int.fromString(s)->Option.getExn
  let schema = () => {
    let schemaRegex = %re("/\?(\d+)\(((?:\d+\s*)*)\)/")
    switch execRe(schemaRegex) {
    | Some([idStr, allowedStr], l) => {
        let schematic = readInt(idStr)
        let allowed =
          allowedStr
          ->String.trim
          ->String.splitByRegExp(%re("/\s+/"))
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
    let varLitRegex = %re("/^\$\\(\d+)/")
    let varScopeRegex = %re("/^\$([a-zA-Z]\w*)/")
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
  switch execRe(%re(`/^\s*"/`)) {
  | Some([], l) => pos := l
  | Some(_) => error("leading whitespace regex error")
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
