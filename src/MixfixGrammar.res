type assoc = Left | Right | NonAssoc
type holePos = Self | Tighter
type part = Lit(string) | Hole(holePos)

type opDecl = {
  category: string,
  parts: array<part>,
  assoc: assoc,
  name: string,
}

type grammar = {
  categories: array<string>,
  tighterThan: array<(string, string)>, // (tighter, looser)
  ops: array<opDecl>,
}

type compiled = {
  ops: array<opDecl>,
  byName: Dict.t<opDecl>,
  immediateTighter: Dict.t<array<string>>,
  reserved: Belt.Set.String.t,
  roots: array<string>,
}

let empty: grammar = {categories: [], tighterThan: [], ops: []}

let append = (a: grammar, b: grammar): grammar => {
  categories: Array.concat(a.categories, b.categories),
  tighterThan: Array.concat(a.tighterThan, b.tighterThan),
  ops: Array.concat(a.ops, b.ops),
}

let opFromName = (category: string, name: string, ~assoc: assoc=NonAssoc): grammar => {
  let segs = name->String.split("_")
  let parts = []
  segs->Array.forEachWithIndex((seg, i) => {
    if i > 0 {
      parts->Array.push(Hole(Tighter))
    }
    if seg != "" {
      parts->Array.push(Lit(seg))
    }
  })
  let parts = switch assoc {
  | Right =>
    let n = Array.length(parts)
    parts->Array.mapWithIndex((p, i) => i == n - 1 ? Hole(Self) : p)
  | Left | NonAssoc => parts
  }
  {categories: [category], tighterThan: [], ops: [{category, parts, assoc, name}]}
}

let tighterThanDecl = (tighter: string, looser: string): grammar => {
  categories: [tighter, looser],
  tighterThan: [(tighter, looser)],
  ops: [],
}

// The only place this can fail is cycle detection in `tighterThan`.
let compile = (g: grammar): result<compiled, string> => {
  let immediateTighter = Dict.make()
  g.tighterThan->Array.forEach(((tighter, looser)) => {
    let existing = immediateTighter->Dict.get(looser)->Option.getOr([])
    immediateTighter->Dict.set(looser, Array.concat(existing, [tighter]))
  })

  let rec hasCycle = (cat, visiting, done_) =>
    if Belt.Set.String.has(visiting, cat) {
      true
    } else if Belt.Set.String.has(done_, cat) {
      false
    } else {
      let visiting' = Belt.Set.String.add(visiting, cat)
      immediateTighter->Dict.get(cat)->Option.getOr([])->Array.some(t => hasCycle(t, visiting', done_))
    }

  let allCategories =
    g.categories->Array.reduce([], (acc, c) => acc->Array.includes(c) ? acc : Array.concat(acc, [c]))
  let cyclic = allCategories->Array.some(c => hasCycle(c, Belt.Set.String.empty, Belt.Set.String.empty))

  if cyclic {
    Error("tighterThan relation contains a cycle")
  } else {
    let madeATighterOf = g.tighterThan->Array.map(((tt, _)) => tt)->Belt.Set.String.fromArray
    let roots = allCategories->Array.filter(c => !Belt.Set.String.has(madeATighterOf, c))
    let reserved =
      g.ops
      ->Array.flatMap(op =>
        op.parts->Array.filterMap(p => switch p { | Lit(s) => Some(s) | Hole(_) => None })
      )
      ->Belt.Set.String.fromArray
    let byName = Dict.make()
    // NOTE: shadowing of operation names here... hopefully not an issue
    g.ops->Array.forEach(op => byName->Dict.set(op.name, op))
    Ok({ops: g.ops, byName, immediateTighter, reserved, roots})
  }
}
// Incrementally merge two already-compiled grammars. Cheaper than
// uncompile/append/compile.
//
// Collisions: A `tighterThan` edge from `b` that would
// close a cycle in the merged graph is silently dropped instead.
let combine = (a: compiled, b: compiled): compiled => {
  let ops = Array.concat(a.ops, b.ops)
  let byName = a.byName->Dict.copy
  b.byName->Dict.toArray->Array.forEach(((k, v)) => byName->Dict.set(k, v))
  let reserved = Belt.Set.String.union(a.reserved, b.reserved)

  let immediateTighter = a.immediateTighter->Dict.copy

  let reachable = (from: string, to_: string): bool => {
    let rec go = (frontier: array<string>, seen: Belt.Set.String.t) =>
      frontier->Array.some(c => c == to_) ||
        {
          let next =
            frontier
            ->Array.flatMap(c => immediateTighter->Dict.get(c)->Option.getOr([]))
            ->Array.filter(c => !Belt.Set.String.has(seen, c))
          Array.length(next) == 0
            ? false
            : go(next, next->Array.reduce(seen, (s, c) => Belt.Set.String.add(s, c)))
        }
    go([from], Belt.Set.String.empty)
  }

  // Add b's edges one at a time, checking reachability against the
  // graph.
  b.immediateTighter->Dict.toArray->Array.forEach(((looser, tighters)) =>
    tighters->Array.forEach(tighter => {
      if !reachable(looser, tighter) {
        let existing = immediateTighter->Dict.get(looser)->Option.getOr([])
        immediateTighter->Dict.set(looser, Array.concat(existing, [tighter]))
      }
      // else: this specific edge would close a cycle
    })
  )

  let madeATighterOf =
    immediateTighter->Dict.toArray->Array.flatMap(((_, ts)) => ts)->Belt.Set.String.fromArray
  let allCategories =
    Array.concat(
      ops->Array.map(op => op.category),
      immediateTighter->Dict.toArray->Array.flatMap(((looser, ts)) => Array.concat([looser], ts)),
    )->Array.reduce([], (acc, c) => acc->Array.includes(c) ? acc : Array.concat(acc, [c]))
  let roots = allCategories->Array.filter(c => !Belt.Set.String.has(madeATighterOf, c))

  {ops, byName, immediateTighter, reserved, roots}
}


let emptyCompiled: compiled = {
  ops: [],
  byName: Dict.make(),
  immediateTighter: Dict.make(),
  reserved: Belt.Set.String.empty,
  roots: [],
}


let assocKeywordRE = %re("/^(infixl|infixr|infix|mixfix)\b/")
let opNameRE = %re("/^[^\s.()\[\]][^\s.()\[\]]*/")

let takeOpName = (s: string): option<(string, string)> => {
  let s = MixfixLex.skipWs(s)
  switch opNameRE->RegExp.exec(s) {
  | None => None
  | Some(res) =>
    switch res[0] {
    | Some(Some(tok)) => Some((tok, MixfixLex.sliceToEnd(s, ~start=String.length(tok))))
    | _ => None
    }
  }
}

let expectKeyword = (s: string, kw: string): option<string> => {
  let s = MixfixLex.skipWs(s)
  switch MixfixLex.takeIdent(s) {
  | Some((tok, rest)) if tok == kw => Some(rest)
  | _ => None
  }
}

// One declaration -> the grammar fragment it denotes, plus leftover input.
let parseDecl = (input: string): result<(grammar, string), string> => {
  let input = MixfixLex.skipWs(input)
  switch assocKeywordRE->RegExp.exec(input) {
  | Some(res) =>
    switch res[0] {
    | Some(Some(kw)) =>
      let assoc = switch kw {
      | "infixl" => Left
      | "infixr" => Right
      | "infix" | "mixfix" => NonAssoc
      | _ => NonAssoc
      }
      let rest = MixfixLex.sliceToEnd(input, ~start=String.length(kw))
      switch MixfixLex.takeIdent(MixfixLex.skipWs(rest)) {
      | None => Error(`expected a category name after "${kw}"`)
      | Some((category, rest')) =>
        switch takeOpName(rest') {
        | None => Error(`expected an operator name after category "${category}"`)
        | Some((name, rest'')) =>
          if !String.includes(name, "_") {
            Error(`operator name "${name}" must contain at least one "_" hole`)
          } else {
            Ok((opFromName(category, name, ~assoc), rest''))
          }
        }
      }
    | _ => Error("malformed declaration keyword")
    }
  | None =>
    switch expectKeyword(input, "tighter") {
    | None => Error(`expected a declaration (infixl/infixr/infix/mixfix/tighter) at: ${input}`)
    | Some(rest) =>
      switch MixfixLex.takeIdent(MixfixLex.skipWs(rest)) {
      | None => Error(`expected a category name after "tighter"`)
      | Some((tighter, rest')) =>
        switch expectKeyword(rest', "than") {
        | None => Error(`expected "than" after "tighter ${tighter}"`)
        | Some(rest'') =>
          switch MixfixLex.takeIdent(MixfixLex.skipWs(rest'')) {
          | None => Error(`expected a category name after "than"`)
          | Some((looser, rest''')) => Ok((tighterThanDecl(tighter, looser), rest'''))
          }
        }
      }
    }
  }
}

let parseDecls = (input: string): result<(grammar, string), string> => {
  let rec go = (input, acc) => {
    let input = MixfixLex.skipWs(input)
    switch parseDecl(input) {
    | Ok((g, rest)) => go(rest, append(acc, g))
    | Error(_) => Ok((acc, input))
    }
  }
  go(input, empty)
}


let assocKeyword = (op: opDecl): string =>
  switch op.assoc {
  | Left => "infixl"
  | Right => "infixr"
  | NonAssoc =>
    switch (op.parts[0], op.parts[Array.length(op.parts) - 1]) {
    | (Some(Hole(_)), Some(Hole(_))) => "infix"
    | _ => "mixfix"
    }
  }

let printOpDecl = (op: opDecl): string => `${assocKeyword(op)} ${op.category} ${op.name}`

let printTighterDecl = ((tighter, looser): (string, string)): string => `tighter ${tighter} than ${looser}`

let prettyPrintGrammar = (g: grammar): string =>
  Array.concat(g.ops->Array.map(printOpDecl), g.tighterThan->Array.map(printTighterDecl))->Array.join("\n")
