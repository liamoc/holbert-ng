type rec t =
  | Symbol({name: string, constructor: bool})
  | Var({idx: int})
  | Schematic({schematic: int})
  | Lam({name: string, body: t})
  | App({func: t, arg: t})

type schematic = int
type meta = string
type subst = Belt.Map.Int.t<t>


type gen = {mutable next: int}

let makeGen = () => {next: 0}

let fresh = (g, ~replacing as _=?) => {
  let s = g.next
  g.next = g.next + 1
  s
}

let seen = (g, s) =>
  if s >= g.next {
    g.next = s + 1
  }

let place = (schematic, ~scope) => {
  let n = Belt.Array.length(scope)
  Belt.Array.reduceWithIndex(scope, Schematic({schematic: schematic}), (acc, _name, i) => App({
    func: acc,
    arg: Var({idx: n - 1 - i}),
  }))
}

// ---------- de Bruijn machinery ----------

let rec upshift = (term, n, ~from=0) =>
  switch term {
  | Symbol(_) | Schematic(_) => term
  | Var({idx}) => idx >= from ? Var({idx: idx + n}) : term
  | Lam({name, body}) => Lam({name, body: upshift(body, n, ~from=from + 1)})
  | App({func, arg}) => App({func: upshift(func, n, ~from), arg: upshift(arg, n, ~from)})
  }

// Simultaneous substitution: replaces Var(from), Var(from+1), ...,
// Var(from+len-1) with values[0..len-1] (each correctly shifted for
// the depth at which it's inserted), and downshifts any free
// variable above that range by len. With from=0 and a single value
// this is ordinary single-variable substitution.
let substDeBruijn = (term, values, ~from=0) => {
  let len = Belt.Array.length(values)
  let rec go = (term, depth) =>
    switch term {
    | Symbol(_) | Schematic(_) => term
    | Var({idx}) =>
      if idx < from + depth {
        term
      } else if idx < from + depth + len {
        upshift(Belt.Array.getExn(values, idx - from - depth), depth, ~from=0)
      } else {
        Var({idx: idx - len})
      }
    | Lam({name, body}) => Lam({name, body: go(body, depth + 1)})
    | App({func, arg}) => App({func: go(func, depth), arg: go(arg, depth)})
    }
  go(term, 0)
}

let rec reduce = term =>
  switch term {
  | Symbol(_) | Var(_) | Schematic(_) => term
  | Lam({name, body}) => Lam({name, body: reduce(body)})
  | App({func, arg}) =>
    let func' = reduce(func)
    let arg' = reduce(arg)
    switch func' {
    | Lam({body}) => reduce(substDeBruijn(body, [arg'], ~from=0))
    | _ => App({func: func', arg: arg'})
    }
  }

let rec concrete = term => true /* term =>
  switch term {
  | Symbol(_) | Var(_) => true
  | Schematic(_) => false
  | Lam({body}) => concrete(body)
  | App({func, arg}) => concrete(func) && concrete(arg)
  }*/


let rec structEqual = (a, b) =>
  switch (a, b) {
  | (Symbol({name: n1, constructor: c1}), Symbol({name: n2, constructor: c2})) =>
    n1 == n2 && c1 == c2
  | (Var({idx: i1}), Var({idx: i2})) => i1 == i2
  | (Schematic({schematic: s1}), Schematic({schematic: s2})) => s1 == s2
  | (Lam({body: b1}), Lam({body: b2})) => structEqual(b1, b2)
  | (App({func: f1, arg: a1}), App({func: f2, arg: a2})) =>
    structEqual(f1, f2) && structEqual(a1, a2)
  | _ => false
  }

let equivalent = (a, b) => structEqual(reduce(a), reduce(b))

// ---------- substitutions ----------

let makeSubst = () => Belt.Map.Int.empty

let rec substitute = (term, s) =>
  switch term {
  | Symbol(_) | Var(_) => term
  | Schematic({schematic}) =>
    switch Belt.Map.Int.get(s, schematic) {
    // recurse so a subst that itself contains chained schematic
    // solutions resolves in one call; assumes s is acyclic (an
    // occurs-check-respecting subst always is).
    | Some(t) => substitute(t, s)
    | None => term
    }
  | Lam({name, body}) => Lam({name, body: substitute(body, s)})
  | App({func, arg}) => App({func: substitute(func, s), arg: substitute(arg, s)})
  }

let mapSubst = (s, f) => Belt.Map.Int.map(s, f)

// Composition: s2 "after" s1 — push s2 into s1's range, then union
// in s2's own bindings. Assumes domains don't genuinely conflict
// (true for substitutions produced while unifying independent
// subterms of the same problem).
let mergeSubsts = (s1, s2) => {
  let s1' = mapSubst(s1, t => substitute(t, s2))
  Belt.Map.Int.merge(s1', s2, (_, a, b) =>
    switch (a, b) {
    | (Some(t), _) => Some(t)
    | (None, Some(t)) => Some(t)
    | (None, None) => None
    }
  )
}

let substEqual = (s1, s2) =>
  Belt.Map.Int.size(s1) == Belt.Map.Int.size(s2) &&
    Belt.Map.Int.every(s1, (k, v) =>
      switch Belt.Map.Int.get(s2, k) {
      | Some(v2) => equivalent(v, v2)
      | None => false
      }
    )

// ---------- unification ----------

// Application spine: head plus args in application order.
let rec spineList = t =>
  switch t {
  | App({func, arg}) =>
    let (h, args) = spineList(func)
    (h, Belt.List.concat(args, list{arg}))
  | _ => (t, list{})
  }


// Is `t` of the form `Schematic(n)[x_i0, ..., x_i(k-1)]` with each
// x_ij a bound variable? Unlike strict Miller-pattern unification,
// the x_ij are *not* required to be distinct — a spine like `?0 n n`
// (arising e.g. from instantiating an eliminator's `P n` premise
// where the same bound variable fills two rule-positions) is
// accepted as a "quasi-pattern". See `makeSolution` for how the
// resulting ambiguity is resolved.
let asPattern = t => {
  let (head, args) = spineList(t)
  switch head {
  | Schematic({schematic}) =>
    let idxs = Belt.List.map(args, a =>
      switch a {
      | Var({idx}) => Some(idx)
      | _ => None
      }
    )
    Belt.List.every(idxs, Belt.Option.isSome)
      ? Some((schematic, idxs->Belt.List.map(Belt.Option.getExn)->Belt.List.toArray))
      : None
  | _ => None
  }
}

// last index j such that arr[j] == p, if any
let lastIndexOf = (arr, p) => {
  let rec go = i =>
    i < 0
      ? None
      : Belt.Array.getExn(arr, i) == p
      ? Some(i)
      : go(i - 1)
  go(Belt.Array.length(arr) - 1)
}

// Build the closed solution `λ x0' .. x(k-1)'. body` for
// `Schematic(n)[spine] := rhs`, given rhs's free vars ⊆ spine.
// spine[j] (an ambient bound-var index) becomes the (k-1-j)-th
// innermost bound variable of the solution, matching how `place`
// applies args left-to-right (outermost lambda binds the first arg).
//
// When a bound variable occurs more than once in `spine` (a
// non-linear/quasi-pattern), occurrences of it in `rhs` are mapped
// to the *rightmost* (last, i.e. innermost-lambda) spine position.
// This is a deliberate choice among several sound solutions, not a 
// canonical one.
let makeSolution = (rhs, spineArr) => {
  let k = Belt.Array.length(spineArr)
  let maxIdx = Belt.Array.reduce(spineArr, -1, (m, i) => max(m, i))
  let values = Belt.Array.makeBy(maxIdx + 1, p =>
    switch lastIndexOf(spineArr, p) {
    | Some(j) => Var({idx: k - 1 - j})
    | None => Symbol({name: "_unused", constructor: false}) // never read, by the scope check
    }
  )
  let body = substDeBruijn(rhs, values, ~from=0)
  let rec wrapLams = (m, b) => m <= 0 ? b : wrapLams(m - 1, Lam({name: "x", body: b}))
  wrapLams(k, body)
}

let rec occurs = (n, t) =>
  switch t {
  | Symbol(_) | Var(_) => false
  | Schematic({schematic}) => schematic == n
  | Lam({body}) => occurs(n, body)
  | App({func, arg}) => occurs(n, func) || occurs(n, arg)
  }

let freeVars = t => {
  let acc = ref(Belt.Set.Int.empty)
  let rec go = (t, depth) =>
    switch t {
    | Symbol(_) | Schematic(_) => ()
    | Var({idx}) =>
      if idx >= depth {
        acc := Belt.Set.Int.add(acc.contents, idx - depth)
      }
    | Lam({body}) => go(body, depth + 1)
    | App({func, arg}) => {
        go(func, depth)
        go(arg, depth)
      }
    }
  go(t, 0)
  acc.contents
}


// Try to solve `a` (assumed reduced) as a pattern for `b` (also
// reduced). Occurs check + scope check (b's free vars ⊆ a's spine).
let tryFlexSolve = (a, b) =>
  switch asPattern(a) {
  | Some((n, spineArr)) =>
    if occurs(n, b) {
      None
    } else {
      let spineSet = Belt.Set.Int.fromArray(spineArr)
      if Belt.Set.Int.subset(freeVars(b), spineSet) {
        Some(Belt.Map.Int.fromArray([(n, makeSolution(b, spineArr))]))
      } else {
        None
      }
    }
  | None => None
  }

// M[spine1] =?= M[spine2], same M, spines differ: keep only the
// positions where they agree, solve M in terms of a smaller fresh
// metavariable applied to just those positions.
let tryFlexFlexSame = (n, spine1, spine2, gen) =>
  switch gen {
  | None => None
  | Some(g) =>
    let k = Belt.Array.length(spine1)
    if k != Belt.Array.length(spine2) {
      None
    } else {
      let agree =
        Belt.Array.range(0, k - 1)->Belt.Array.keep(i =>
          Belt.Array.getExn(spine1, i) == Belt.Array.getExn(spine2, i)
        )
      let n' = fresh(g)
      let body = Belt.Array.reduce(agree, Schematic({schematic: n'}), (acc, i) => App({
        func: acc,
        arg: Var({idx: k - 1 - i}),
      }))
      let rec wrapLams = (m, b) => m <= 0 ? b : wrapLams(m - 1, Lam({name: "x", body: b}))
      Some(Belt.Map.Int.fromArray([(n, wrapLams(k, body))]))
    }
  }


let unifyRigidHeaded = (t1, t2, gen, unifyStep) => {
  let (h1, args1) = spineList(t1)
  let (h2, args2) = spineList(t2)
  let headsMatch = switch (h1, h2) {
  | (Symbol({name: n1, constructor: c1}), Symbol({name: n2, constructor: c2})) =>
    n1 == n2 && c1 == c2
  | (Var({idx: i1}), Var({idx: i2})) => i1 == i2
  | _ => false
  }
  let a1 = Belt.List.toArray(args1)
  let a2 = Belt.List.toArray(args2)
  if headsMatch && Belt.Array.length(a1) == Belt.Array.length(a2) {
    let n = Belt.Array.length(a1)
    let rec loop = (i, acc) =>
      if i >= n {
        Some(acc)
      } else {
        switch unifyStep(
          substitute(Belt.Array.getExn(a1, i), acc),
          substitute(Belt.Array.getExn(a2, i), acc),
          gen,
        ) {
        | None => None
        | Some(s) => loop(i + 1, mergeSubsts(acc, s))
        }
      }
    loop(0, makeSubst())
  } else {
    None
  }
}

let rec unifyStep = (t1, t2, gen) => {
  let t1 = reduce(t1)
  let t2 = reduce(t2)
  if structEqual(t1, t2) {
    Some(makeSubst())
  } else {
    switch (t1, t2) {
    | (Lam({body: b1}), Lam({body: b2})) => unifyStep(b1, b2, gen)
    // eta: unify λ.b1 against t2 by comparing b1 to (t2 shifted) applied to a fresh bound var
    | (Lam({body: b1}), _) => unifyStep(b1, App({func: upshift(t2, 1), arg: Var({idx: 0})}), gen)
    | (_, Lam({body: b2})) => unifyStep(App({func: upshift(t1, 1), arg: Var({idx: 0})}), b2, gen)
    | _ =>
      switch (asPattern(t1), asPattern(t2)) {
      | (Some((n1, s1)), Some((n2, s2))) if n1 == n2 => tryFlexFlexSame(n1, s1, s2, gen)
      | _ =>
        switch tryFlexSolve(t1, t2) {
        | Some(s) => Some(s)
        | None =>
          switch tryFlexSolve(t2, t1) {
          | Some(s) => Some(s)
          | None => unifyRigidHeaded(t1, t2, gen, unifyStep)
          }
        }
      }
    }
  }
}

let unify = (t1, t2, ~gen=?) => {
  Console.log(("U",t1,t2))
  switch unifyStep(t1, t2, gen) {
  | Some(s) => {Console.log(s); Seq.cons(s, Seq.empty)}
  | None => Seq.empty
  }
}

let prettyPrintVar = (idx: int, scope: array<string>) =>
  switch scope[idx] {
  | Some(n) if Array.indexOf(scope, n) == idx => n
  | _ => "\\"->String.concat(String.make(idx))
  }  
let rec strip = (term: t): (t, array<t>) => {
  switch term {
  | App({func, arg}) =>
    let (peeledFunc, peeledArgs) = strip(func)
    (peeledFunc, Array.concat(peeledArgs, [arg]))
  | _ => (term, [])
  }
}  
let rec stripLam = (it: t): (array<string>, t) =>
  switch it {
  | Lam({name, body}) =>
    let (names, body) = stripLam(body)
    (Array.concat([name], names), body)
  | _ => ([], it)
  }
let rec unstrip = (term: t, args: array<t>): t => {
    if args->Array.length == 0 {
      term
    } else {
      let head = args[0]->Option.getExn
      let rest = args->Array.sliceToEnd(~start=1)
      unstrip(App({func: term, arg: head}), rest)
    }
  }  
let rec prettyPrint = (it: t, ~scope: array<string>) =>
  switch it {
  | Symbol({name, constructor}) =>
    if constructor {
      String.concat("@", name)
    } else {
      name
    }
  | Var({idx}) => prettyPrintVar(idx, scope)
  | Schematic({schematic}) => "?"->String.concat(String.make(schematic))
  | Lam(_) =>
    let (names, body) = stripLam(it)
    let (func, args) = strip(body)
    let bodies = Array.concat([func], args)
    let innerScope = Array.concat(Array.toReversed(names), scope)
    "("
    ->String.concat(Array.join(names->Array.map(name => String.concat(name, ".")), " "))
    ->String.concat(" ")
    ->String.concat(Array.join(bodies->Array.map(e => prettyPrint(e, ~scope=innerScope)), " "))
    ->String.concat(")")
  | App(_) =>
    let (func, args) = strip(it)
    "("
    ->String.concat(prettyPrint(func, ~scope))
    ->String.concat(" ")
    ->String.concat(Array.join(args->Array.map(e => prettyPrint(e, ~scope)), " "))
    ->String.concat(")")
  }
let prettyPrintMeta = (str: string) => {
    String.concat(str, ".")
  }
let prettyPrintSubst = (sub: subst, ~scope: array<string>) =>
  Util.prettyPrintIntMap(sub, ~showV=t => prettyPrint(t, ~scope))
  
  
  
  
let nameRES = "^([^\\s.\\[\\]()]+)\\."
let symbolRES = "^([^\\s.\\[\\]()]+)"
exception ParseError(string)
type token =
  | LParen
  | RParen
  | VarT(int)
  | SchematicT(int)
  | ConsT(string)
  | NameT(string)
  | AtomT(string)
  | EOF
let varRegexpString = "^\\\\([0-9]+)"
let schematicRegexpString = "^\\?([0-9]+)"

let scopeVarToken = (str: string, scope: array<string>): option<(int, string)> => {
  let result = ref(None)
  scope->Array.forEachWithIndex((name, idx) => {
    let len = String.length(name)
    let matches =
      String.slice(str, ~start=0, ~end=len) == name &&
        switch str->String.charAt(len) {
        | "" | " " | "\t" | "\n" | "\r" | "[" | "]" | "(" | ")" => true
        | _ => false
        }
    if result.contents == None && matches {
      result := Some((idx, str->String.sliceToEnd(~start=len)))
    }
  })
  result.contents
}
let tokenize = (str0: string, ~scope: array<string>, ~gen=?): (token, string) => {
  let str = str0->String.trimStart
  if str->String.length == 0 {
    (EOF, "")
  } else {
    let rest = () => str->String.sliceToEnd(~start=1)
    switch str->String.charAt(0) {
    | "(" => (LParen, rest())
    | ")" => (RParen, rest())
    | "\\" => {
        let re = RegExp.fromStringWithFlags(varRegexpString, ~flags="y")
        switch re->RegExp.exec(str) {
        | None => throw(ParseError("invalid variable"))
        | Some(res) =>
          switch RegExp.Result.matches(res) {
          | [n] => (
              VarT(n->Int.fromString->Option.getExn),
              String.sliceToEnd(str, ~start=RegExp.lastIndex(re)),
            )
          | _ => throw(ParseError("invalid variable"))
          }
        }
      }
    | "?" => {
        let re = RegExp.fromStringWithFlags(schematicRegexpString, ~flags="y")
        switch re->RegExp.exec(str) {
        | None => throw(ParseError("invalid schematic"))
        | Some(res) =>
          switch RegExp.Result.matches(res) {
          | [n] => (
              SchematicT(n->Int.fromString->Option.getExn),
              String.sliceToEnd(str, ~start=RegExp.lastIndex(re)),
            )
          | _ => throw(ParseError("invalid schematic"))
          }
        }
      }
    | _ => {
        let reName = RegExp.fromStringWithFlags(symbolRES, ~flags="y")
        switch scopeVarToken(str, scope) {
        | Some((idx, rest)) => (VarT(idx), rest)
        | None => switch reName->RegExp.exec(str) {
          | Some(res) => {
            let rest = String.sliceToEnd(str, ~start=RegExp.lastIndex(reName))
            switch RegExp.Result.matches(res) {
            | [n] => if n->String.charAt(0)=="@" {
                (ConsT(n), rest)
              } else if rest->String.charAt(0)=="." {
                (NameT(n), rest->String.sliceToEnd(~start=1))
              } else {
                (AtomT(n), rest)
              }
         
            | _ => throw(ParseError("invalid symbol"))
            }
          }
          | None => throw(ParseError("unrecognised input"))
          }
        }
      }
    }
  }
}
type rec simple =
  | ListS({xs: array<simple>})
  | AtomS({name: string, constructor: bool})
  | VarS({idx: int})
  | SchematicS({schematic: int})
  | LambdaS({name: string, body: simple})
let rec parseSimple = (str: string, ~scope: array<string>, ~gen=?): (simple, string) => {
  let (t0, rest) = tokenize(str, ~scope, ~gen?)
  switch t0 {
  | LParen => {
      let (t1, rest1) = tokenize(rest, ~scope, ~gen?)
      switch t1 {
      | NameT(name) => {
          let (result, rest2) = parseSimple(
            "("->String.concat(rest1),
            ~scope=Array.concat([name], scope),
            ~gen?,
          )
          (LambdaS({name, body: result}), rest2)
        }
      | RParen => (ListS({xs: []}), rest1)
      | _ => {
          let (head, rest2) = parseSimple(rest, ~scope, ~gen?)
          let (tail, rest3) = parseSimple("("->String.concat(rest2), ~scope, ~gen?)
          switch tail {
          | ListS({xs}) => (ListS({xs: Array.concat([head], xs)}), rest3)
          | _ => throw(Util.Unreachable("bug"))
          }
        }
      }
    }
  | RParen => throw(ParseError("unexpected right parenthesis"))
  | VarT(idx) => (VarS({idx: idx}), rest)
  | SchematicT(schematic) => (SchematicS({schematic: schematic}), rest)
  | AtomT(name) => (AtomS({name, constructor: false}), rest)
  | ConsT(name) => (AtomS({name, constructor: true}), rest)
  | NameT(name) => {
      let (result, rest1) = parseSimple(rest, ~scope=Array.concat([name], scope), ~gen?)
      (LambdaS({name, body: result}), rest1)
    }
  | EOF => throw(ParseError("unexpected end of file"))
  }
}
let rec parseAll = (simple: simple, ~gen=?): t => {
  switch simple {
  | ListS({xs}) => {
      let ts = xs->Array.map(x => parseAll(x, ~gen?))
      if ts->Array.length == 0 {
        throw(ParseError("empty list"))
      } else {
        ts
        ->Array.sliceToEnd(~start=1)
        ->Array.reduce(ts[0]->Option.getExn, (acc, x) => App({func: acc, arg: x}))
      }
    }
  | AtomS({name, constructor}) => Symbol({name, constructor})
  | VarS({idx}) => Var({idx: idx})
  | SchematicS({schematic}) =>
    switch gen {
    | Some(g) => {
        seen(g, schematic)
        Schematic({schematic: schematic})
      }
    | None => throw(ParseError("Schematics not allowed here"))
    }
  | LambdaS({name, body}) =>
    Lam({
      name,
      body: parseAll(body, ~gen?),
    })
  }
}
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
  try {
    let (simple, rest) = parseSimple(str, ~scope, ~gen?)
    Ok((parseAll(simple, ~gen?), rest))
  } catch {
  | ParseError(msg) => Error(msg)
  }
}
let mapTerms = (t, f) => f(t)