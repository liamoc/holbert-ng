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
  Belt.Array.reduceWithIndex(scope, Schematic({schematic: schematic}), (acc, _name, i) => App({
    func: acc,
    arg: Var({idx: i}),
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

let rec betaReduce = term =>
  switch term {
  | Symbol(_) | Var(_) | Schematic(_) => term
  | Lam({name, body}) => Lam({name, body: betaReduce(body)})
  | App({func, arg}) =>
    let func' = betaReduce(func)
    let arg' = betaReduce(arg)
    switch func' {
    | Lam({body}) => betaReduce(substDeBruijn(body, [arg'], ~from=0))
    | _ => App({func: func', arg: arg'})
    }
  }


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


// Does Var(depth) occur free in `t`?
let rec occursFree = (t, depth) =>
  switch t {
  | Symbol(_) | Schematic(_) => false
  | Var({idx}) => idx == depth
  | Lam({body}) => occursFree(body, depth + 1)
  | App({func, arg}) => occursFree(func, depth) || occursFree(arg, depth)
  }

// Inverse of upshift(_, 1): only valid when Var(0) doesn't occur free
// (see occursFree above).
let downshift1 = t => {
  let rec go = (t, depth) =>
    switch t {
    | Symbol(_) | Schematic(_) => t
    | Var({idx}) => idx > depth ? Var({idx: idx - 1}) : t
    | Lam({name, body}) => Lam({name, body: go(body, depth + 1)})
    | App({func, arg}) => App({func: go(func, depth), arg: go(arg, depth)})
    }
  go(t, 0)
}

// Contract eta-redexes bottom-up: Lam(App(f, Var(0))) ~> f, whenever
// Var(0) isn't free in f. Assumes its input is already beta-normal
// (produced by `betaReduce`).
// Contraction can't introduce new beta redexes, so a single bottom-up 
// pass suffices.
let rec etaNormalise = t => {
  let t' = switch t {
  | Symbol(_) | Var(_) | Schematic(_) => t
  | Lam({name, body}) => Lam({name, body: etaNormalise(body)})
  | App({func, arg}) => App({func: etaNormalise(func), arg: etaNormalise(arg)})
  }
  switch t' {
  | Lam({body: App({func, arg: Var({idx: 0})})}) if !occursFree(func, 0) => downshift1(func)
  | _ => t'
  }
}

let reduce = t => etaNormalise(betaReduce(t))

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

let rec strip = (term: t): (t, array<t>) => {
  switch term {
  | App({func, arg}) =>
    let (peeledFunc, peeledArgs) = strip(func)
    (peeledFunc, Array.concat(peeledArgs, [arg]))
  | _ => (term, [])
  }
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




let concrete = t => {
  let (head, _args) = strip(t)
  switch head {
  | Schematic(_) => false
  | _ => true
  }
}


// Is `t` of the form `Schematic(n)[x_i0, ..., x_i(k-1)]` with each
// x_ij a bound variable? Unlike strict Miller-pattern unification,
// the x_ij are *not* required to be distinct — a spine like `?0 n n`
// (arising e.g. from instantiating an eliminator's `P n` premise
// where the same bound variable fills two rule-positions) is
// accepted as a "quasi-pattern". See `makeSolution` for how the
// resulting ambiguity is resolved.
let asPattern = t => {
  let (head, args) = strip(t)
  switch head {
  | Schematic({schematic}) =>
    let idxs = Array.map(args, a =>
      switch a {
      | Var({idx}) => Some(idx)
      | _ => None
      }
    )
    Array.every(idxs, Belt.Option.isSome)
      ? Some((schematic, Array.map(idxs, Belt.Option.getExn)))
      : None
  | _ => None
  }
}

// last index j such that arr[j] == p, if any
let lastIndexOf = (arr, p) => {
  let rec go = i =>
    i < 0
      ? None
      : arr->Belt.Array.getExn(i) == p
      ? Some(i)
      : go(i - 1)
  go(arr->Array.length - 1)
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
  let k = spineArr->Array.length
  let maxIdx = spineArr->Array.reduce(-1, (m, i) => max(m, i))
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
    let k = spine1->Array.length
    if k != spine2->Array.length {
      None
    } else {
      let agree =
        Belt.Array.range(0, k - 1)->Belt.Array.keep(i =>
          spine1->Belt.Array.getExn(i) == spine2->Belt.Array.getExn(i)
        )
      let n' = fresh(g)
      let body = agree->Array.reduce(Schematic({schematic: n'}), (acc, i) => App({
        func: acc,
        arg: Var({idx: k - 1 - i}),
      }))
      let rec wrapLams = (m, b) => m <= 0 ? b : wrapLams(m - 1, Lam({name: "x", body: b}))
      Some(Belt.Map.Int.fromArray([(n, wrapLams(k, body))]))
    }
  }


let unifyRigidHeaded = (t1, t2, gen, unifyStep) => {
  let (h1, a1) = strip(t1)
  let (h2, a2) = strip(t2)
  let headsMatch = switch (h1, h2) {
  | (Symbol({name: n1, constructor: c1}), Symbol({name: n2, constructor: c2})) =>
    n1 == n2 && c1 == c2
  | (Var({idx: i1}), Var({idx: i2})) => i1 == i2
  | _ => false
  }
  if headsMatch && Array.length(a1) == Array.length(a2) {
    let n = Array.length(a1)
    let rec loop = (i, acc) =>
      if i >= n {
        Some(acc)
      } else {
        switch unifyStep(
          substitute(a1->Belt.Array.getExn(i), acc),
          substitute(a2->Belt.Array.getExn(i), acc),
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
  let t1 = betaReduce(t1)
  let t2 = betaReduce(t2)
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
  switch unifyStep(t1, t2, gen) {
  | Some(s) => Seq.cons(s, Seq.empty)
  | None => Seq.empty
  }
}

type grammar = MixfixGrammar.compiled

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


let mapTerms = (t, f) => f(t)


type step = Func | Arg | Body
type path = array<step>
let rec locateFrom = (term: t, path: path, i: int, scope: array<meta>): option<(t, array<meta>)> =>
  if i >= Belt.Array.length(path) {
    Some((term, scope))
  } else {
    switch (term, Belt.Array.getExn(path, i)) {
    | (App({func}), Func) => locateFrom(func, path, i + 1, scope)
    | (App({arg}), Arg) => locateFrom(arg, path, i + 1, scope)
    | (Lam({name, body}), Body) => locateFrom(body, path, i + 1, Belt.Array.concat(scope, [name]))
    | _ => None
    }
  }
let locate = (term, path) => locateFrom(term, path, 0, [])

let rec replaceAtFrom = (term: t, path: path, i: int, replacement: t): t =>
  if i >= Belt.Array.length(path) {
    replacement
  } else {
    switch (term, Belt.Array.getExn(path, i)) {
    | (App({func, arg}), Func) => App({func: replaceAtFrom(func, path, i + 1, replacement), arg})
    | (App({func, arg}), Arg) => App({func, arg: replaceAtFrom(arg, path, i + 1, replacement)})
    | (Lam({name, body}), Body) => Lam({name, body: replaceAtFrom(body, path, i + 1, replacement)})
    | _ => term
    }
  }
let replaceAt = (term, path, replacement) => replaceAtFrom(term, path, 0, replacement)

let mkEquation = (a, b) =>
  App({func: App({func: Symbol({name: "=", constructor: false}), arg: a}), arg: b})

let asEquation = (t: t): option<(t, t)> =>
  switch t {
  | App({func: App({func: Symbol({name: "=", constructor: false}), arg: a}), arg: b}) => Some((a, b))
  | _ => None
  }
  
let positions = (term: t): array<(path, t, array<string>)> => {
  let rec go = (term, scope, path) => {
    let here = (path, term, scope)
    let children = switch term {
    | App({func, arg}) =>
      Array.concat(go(func, scope, Array.concat(path, [Func])), go(arg, scope, Array.concat(path, [Arg])))
    | Lam({name, body}) => go(body, Array.concat(scope, [name]), Array.concat(path, [Body]))
    | Symbol(_) | Var(_) | Schematic(_) => []
    }
    Array.concat([here], children)
  }
  go(term, [], [])
}  

let prettyPrintStep = (s : step) => switch s {
  | Func => "F"
  | Arg => "A"
  | Body => "B"
}

let prettyPrintPath = (p : path) => p->Array.map(prettyPrintStep)->Array.join("")

let parsePath = (str:string) => {
  let re = RegExp.fromStringWithFlags("([FAB]*)", ~flags="y")
  let toStep = (ch) => 
    switch ch {
    | "F" => Func
    | "A" => Arg
    | _   => Body    
    }
  switch re->RegExp.exec(str) {
  | Some(res) => {
      let rest = String.sliceToEnd(str, ~start=RegExp.lastIndex(re))
      switch RegExp.Result.matches(res) {
      | [] => ([],str)
      | [n] => (n->String.split("")->Array.map(toStep),rest)
      | _ => ([],str)
      }
    }    
  | _ => ([],str)
  }
}

let emptyGrammar = MixfixGrammar.emptyCompiled
let combineGrammars = MixfixGrammar.combine

module ParseLeaf: MixfixParser.PARSE_LEAF
  with type term = t
  and type meta = string
  and type gen = gen = {
  type term = t
  type meta = meta
  type gen = gen

  let debruijnRE = %re("/^\\(\d+)/")
  let schematicRE = %re("/^\?(\d+)/")

  let parseLeaf = (input, ~reserved, ~scope, ~gen=?, ~recur) => {
    let input = MixfixLex.skipWs(input)
    switch debruijnRE->RegExp.exec(input) {
    | Some(res) =>
      switch (res[0], res[1]) {
      | (Some(Some(whole)), Some(Some(numStr))) =>
        switch Int.fromString(numStr) {
        | None => Error("invalid de Bruijn index")
        | Some(idx) =>
          idx < Array.length(scope)
            ? Ok((Var({idx: idx}), MixfixLex.sliceToEnd(input, ~start=String.length(whole))))
            : Error(
                `de Bruijn index \\${numStr} out of scope (only ${Int.toString(
                    Array.length(scope),
                  )} binders in scope)`,
              )
        }
      | _ => Error("malformed de Bruijn index")
      }
    | None =>
      if MixfixLex.charAt(input, 0) == "`" {
        switch MixfixLex.takeIdent(MixfixLex.sliceToEnd(input, ~start=1)) {
        | Some((name, rest)) => Ok((Symbol({name, constructor: false}), rest))
        | None => Error("expected identifier after `")
        }
      } else if MixfixLex.charAt(input, 0) == "(" {
        let inner = MixfixLex.sliceToEnd(input, ~start=1)
        switch MixfixLex.takeIdent(inner) {
        | Some((name, afterName)) if MixfixLex.charAt(MixfixLex.skipWs(afterName), 0) == "." =>
          let bodyInput = MixfixLex.sliceToEnd(MixfixLex.skipWs(afterName), ~start=1)
          switch recur(bodyInput, ~scope=Array.concat([name], scope), ~gen?) {
          | Error(e) => Error(e)
          | Ok((body, rest)) =>
            let rest = MixfixLex.skipWs(rest)
            MixfixLex.charAt(rest, 0) == ")"
              ? Ok((Lam({name, body}), MixfixLex.sliceToEnd(rest, ~start=1)))
              : Error("expected closing paren after lambda body")
          }
        | _ =>
          switch recur(inner, ~scope, ~gen?) {
          | Error(e) => Error(e)
          | Ok((body, rest)) =>
            let rest = MixfixLex.skipWs(rest)
            MixfixLex.charAt(rest, 0) == ")"
              ? Ok((body, MixfixLex.sliceToEnd(rest, ~start=1)))
              : Error("expected closing paren" + rest)
          }
        }
      } else {
        switch schematicRE->RegExp.exec(input) {
        | Some(res) =>
          switch (res[0], res[1]) {
          | (Some(Some(whole)), Some(Some(numStr))) =>
            switch Int.fromString(numStr) {
            | None => Error("invalid schematic index")
            | Some(n) =>
              switch gen {
              | Some(g') => seen(g', n)
              | None => ()
              }
              Ok((
                Schematic({schematic: n}),
                MixfixLex.sliceToEnd(input, ~start=String.length(whole)),
              ))
            }
          | _ => Error("malformed schematic")
          }
        | None =>
          if MixfixLex.charAt(input, 0) == "@" {
            switch MixfixLex.takeIdent(MixfixLex.sliceToEnd(input, ~start=1)) {
            | Some((name, rest)) => Ok((Symbol({name, constructor: true}), rest))
            | None => Error("expected constructor name after @")
            }
          } else {
            switch MixfixLex.takeIdent(input) {
            | None => Error(`expected a term at: ${input}`)
            | Some((name, _)) if Belt.Set.String.has(reserved, name) =>
              Error(`unexpected reserved word "${name}"`)
            | Some((name, rest)) =>
              switch Belt.Array.getIndexBy(scope, x => x == name) {
              | Some(pos) => Ok((Var({idx: pos}), rest))
              | None => Ok((Symbol({name, constructor: false}), rest))
              }
            }
          }
        }
      }
    }
  }

  let mkApp = (f, a) => App({func: f, arg: a})
  let mkOpHead = (~name) => Symbol({name, constructor: false})
}

module Parser = MixfixParser.Make(ParseLeaf)
let parse = Parser.parse


module PrintLeaf = (O: MixfixPrinter.PRINT_TARGET) => {
  
  type term = t
  type meta = meta
  type out = O.out

  let printLeaf = (term, localIdx, scope, _ctx, ~reserved, ~recur) =>
    switch term {
    | Lam({name, body}) =>
      Some(
        O.seq([
          O.leaf(~kind="lambda-punct", "("),
          O.leaf(~kind="binder", name),
          O.leaf(~kind="lambda-punct", ". "),
          recur(body, localIdx+1, Array.concat( [name],scope), MixfixPrinter.Top),
          O.leaf(~kind="lambda-punct", ")"),
        ]),
      )
    | Var({idx}) =>
      Some(
        switch scope[idx] {
        | None => O.leaf(~kind="var-debruijn", `\\${Int.toString(idx)}`)
        | Some(name) =>
          let resolvedIdx = Belt.Array.getIndexBy(scope, x => x == name)
          let varType = idx >= localIdx ? "metavar" : "boundvar"
          resolvedIdx == Some(idx)
            ? O.leaf(~kind=varType, name)
            : O.leaf(~kind="var-debruijn", `\\${Int.toString(idx)}`)
        },
      )
    | Schematic({schematic}) => Some(O.leaf(~kind="schematic", `?${Int.toString(schematic)}`))
    | Symbol({name, constructor}) =>
      Some(
        constructor
          ? O.leaf(~kind="constructor", `@${name}`)
          : Belt.Set.String.has(reserved, name)
            ? O.leaf(~kind="symbol-escaped", `\`${name}`)
            : O.leaf(~kind="symbol", name),
      )
    | App(_) => None
    }

  let tryStrip = term => {
    let (head, args) = strip(term)
    if (args->Array.length > 0) {
      Some((head,args))
    } else {
      None
    }
  }

  let tryOpHead = head =>
    switch head {
    | Symbol({name, constructor: false}) => Some(name)
    | _ => None
    }

}

module StringPrinter = MixfixPrinter.Make(MixfixPrinter.StringTarget, PrintLeaf(MixfixPrinter.StringTarget))


let prettyPrint = (term, ~grammar, ~scope) => StringPrinter.prettyPrintWithGrammar(term,~parentheses=true,~grammar,~scope)
