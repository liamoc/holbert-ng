module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

type pos = {line: int, col: int, idx: int}
type located<'a> = {content: 'a, loc: pos}
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
type token =
  | StringLit(string)
  | VarLit(int)
  | SchemaLit({id: int, allowed: array<int>})
  | Concat
  | LParen
  | RParen
type remaining = string
type errorMessage = string
let parse: (string, ~scope: array<meta>, ~gen: gen=?) => result<(t, string), string> = (
  str: string,
  ~scope: array<string>,
  ~gen=?,
) => {
  let error = (loc: pos, msg: string) => {
    let codeAroundLoc = String.slice(str, ~start=loc.idx, ~end=loc.idx + 5)
    Error(`${Int.toString(loc.line)}:${Int.toString(loc.col)} (${codeAroundLoc})...: ${msg}`)
  }

  let execRe = (re, str) => {
    re
    ->RegExp.exec(str)
    ->Option.map(result => {
      open RegExp.Result
      (matches(result), fullMatch(result)->String.length)
    })
  }

  let lex: unit => result<array<located<token>>, string> = () => {
    let pos = ref(0)
    let line = ref(1)
    let col = ref(1)
    let loc = () => {
      line: line.contents,
      col: col.contents,
      idx: pos.contents,
    }
    let acc = ref(Ok([]))
    let advance = n => {
      pos := pos.contents + n
      col := col.contents + n
    }
    let advance1 = () => advance(1)
    let newline = () => {
      pos := pos.contents + 1
      line := line.contents + 1
      col := 1
    }
    let add = (token, ~nAdvance=?) => {
      acc.contents
      ->Result.map(acc => {
        Array.push(
          acc,
          {
            content: token,
            loc: {
              line: line.contents,
              col: col.contents,
              idx: pos.contents,
            },
          },
        )
      })
      ->ignore
      Option.map(nAdvance, advance)->ignore
    }
    let error = msg => {
      acc := error(loc(), msg)
    }
    let execRe = re => execRe(re, String.sliceToEnd(str, ~start=pos.contents))
    let stringLit = () => {
      let regex = %re("/^\"([^\"]*)\"/")
      switch execRe(regex) {
      | Some([match], l) => add(StringLit(match), ~nAdvance=l)
      | Some(_) => error("regex string lit error")
      | None => error("expected end quote")
      }
    }
    let readInt = s => Int.fromString(s)->Option.getExn
    let varLit = () => {
      let varRegex = %re("/^\\(\d+)/")
      let schemaRegex = %re("/\\\?(\d+)\(((?:\d+\s*)*)\)/")
      switch execRe(varRegex) {
      | Some([match], l) => add(VarLit(readInt(match)), ~nAdvance=l)
      | Some(_) => error("var lit regex error")
      | None =>
        switch execRe(schemaRegex) {
        | Some([idStr, allowedStr], l) => {
            let id = readInt(idStr)
            let allowed =
              allowedStr
              ->String.trim
              ->String.splitByRegExp(%re("/\s+/"))
              ->Array.keepSome
              ->Array.filter(s => s != "")
              ->Array.map(readInt)
            add(SchemaLit({id, allowed}), ~nAdvance=l)
          }
        | Some(_) => error("schema lit regex error")
        | None => error("expected var or schema")
        }
      }
    }
    let varScope = () => {
      let varRegex = %re("/^([a-zA-Z]\w*)/")
      switch execRe(varRegex) {
      | Some([ident], l) =>
        switch Array.indexOfOpt(scope, ident) {
        | Some(idx) => add(VarLit(idx), ~nAdvance=l)
        | None => error("expected variable in scope")
        }
      | Some(_) => error("var regex error")
      | None => error("unexpected token")
      }
    }
    while {pos.contents < String.length(str) && Result.isOk(acc.contents)} {
      switch String.get(str, pos.contents)->Option.getExn {
      | " " | "\t" | "\r" => advance1()
      | "\n" => newline()
      | "\"" => stringLit()
      | "." => add(Concat, ~nAdvance=1)
      | "(" => add(LParen, ~nAdvance=1)
      | ")" => add(RParen, ~nAdvance=1)
      | "\\" => varLit()
      | _ => varScope()
      }
    }
    acc.contents
  }
  let parseExp = (tokens: array<located<token>>) => {
    let tokenIdx = ref(0)
    let acc: ref<result<array<piece>, (errorMessage, remaining)>> = ref(Ok([]))
    let parens = []
    let add = p => {acc.contents->Result.map(acc => acc->Array.push(p))->ignore}

    while {tokenIdx.contents < Array.length(tokens) && Result.isOk(acc.contents)} {
      let {content: tok, loc} = tokens[tokenIdx.contents]->Option.getExn
      let error = msg => {
        acc :=
          error(loc, msg)->Result.mapError(msg => (msg, String.sliceToEnd(str, ~start=loc.idx)))
      }
      switch tok {
      | StringLit(s) => add(String(s))
      | SchemaLit({id: schematic, allowed}) => add(Schematic({schematic, allowed}))
      | VarLit(idx) => add(Var({idx: idx})) // some reason i'm not allowed to shorthand here?
      | LParen => Array.push(parens, loc)
      | RParen =>
        if Array.length(parens) == 0 {
          error("no matching open paren")
        } else {
          Array.pop(parens)->ignore
        }
      | Concat =>
        switch tokens[tokenIdx.contents + 1] {
        | None | Some({content: RParen}) => error("expected string to follow concatenation")
        | Some(_) => ()
        }
      }
      tokenIdx := tokenIdx.contents + 1
    }
    switch acc.contents {
    | Ok(t) =>
      if Array.length(parens) == 0 {
        Ok(t, "")
      } else {
        let loc = Array.pop(parens)->Option.getExn
        error(loc, "expected closing paren")
      }
    // TODO: decide if this remaining is redundant
    | Error(msg, remaining) => Error(msg)
    }
  }
  switch lex() {
  | Ok(tokens) => parseExp(tokens)
  | Error(msg) => Error(msg)
  }
}
