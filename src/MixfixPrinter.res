open MixfixGrammar

module type PRINT_TARGET = {
  type out
  let leaf: (~kind: string, string) => out
  let seq: array<out> => out
  let spaced: array<out> => out
  let parens: out => out
}

type shape = ALambda | AnAtom | AnApp | AnOp(string)
type printCtx = Top | AppFunc | AppArg | InCategory(string, holePos)

let holeCount = (op: opDecl): int =>
  op.parts->Array.reduce(0, (n, p) => switch p { | Hole(_) => n + 1 | Lit(_) => n })

let effectiveHolePos = (assoc: assoc, i: int, total: int): holePos =>
  switch assoc {
  | Left => i == 0 ? Self : Tighter
  | Right => i == total - 1 ? Self : Tighter
  | NonAssoc => Tighter
  }

let tighterThanStrict = (g: compiled, a: string, b: string): bool => {
  let rec go = (frontier: array<string>, seen: Belt.Set.String.t) =>
    frontier->Array.some(c => c == a) ||
      {
        let next = frontier->Array.flatMap(c => g.immediateTighter->Dict.get(c)->Option.getOr([]))
        let next = next->Array.filter(c => !Belt.Set.String.has(seen, c))
        Array.length(next) == 0
          ? false
          : go(next, next->Array.reduce(seen, (s, c) => Belt.Set.String.add(s, c)))
      }
  go(g.immediateTighter->Dict.get(b)->Option.getOr([]), Belt.Set.String.empty)
}

let needsParens = (g: compiled, shape: shape, ctx: printCtx): bool =>
  switch ctx {
  | Top => false
  | AppFunc => switch shape { | AnOp(_) => true | ALambda | AnAtom | AnApp => false }
  | AppArg => switch shape { | AnAtom | ALambda => false | AnApp | AnOp(_) => true }
  | InCategory(cat, pos) =>
    switch shape {
    | ALambda | AnAtom | AnApp => false
    | AnOp(childCat) => childCat == cat ? pos != Self : !tighterThanStrict(g, childCat, cat)
    }
  }

module type PRINT_LEAF = {
  type term
  type meta
  type out
  // Print any atomic term (i.e. not an application)
  // Return None for compound terms (e.g. app), and use recur for nestings like lambdas.
  let printLeaf: (
    term,
    int,
    array<meta>,
    printCtx,
    ~reserved: Belt.Set.String.t,
    ~recur: (term, int, array<meta>, printCtx) => out,
  ) => option<out>
  // If `term` is an application, split it into a head/spine
  let tryStrip: term => option<(term, array<term>)>
  // If it's a a symbol, get its name
  let tryOpHead: term => option<string>
}

module Make = (O: PRINT_TARGET, L: PRINT_LEAF with type out = O.out) => {
  let rec prettyPrintAt = (g: compiled, term: L.term, localIdx: int, scope: array<L.meta>, ctx: printCtx): O.out =>
    switch L.printLeaf(term, localIdx, scope, ctx, ~reserved=g.reserved, ~recur=(t,i, s, c) => prettyPrintAt(g, t, i, s, c)) {
    | Some(out) => out
    | None =>
      switch L.tryStrip(term) {
      | Some((headTerm, args)) => 
        switch L.tryOpHead(headTerm) {
        | Some(name) => 
          switch g.byName->Dict.get(name) {
          | Some(op) if Array.length(args) >= holeCount(op) => printOpApplication(g, op, args,localIdx, scope, ctx)
          | _ => printPlainApp(g, headTerm, args, localIdx, scope, ctx)
          }
        | None => printPlainApp(g, headTerm, args, localIdx, scope, ctx)
        }
      | None => O.leaf(~kind="unprintable", "?")
      }
    }
  and printPlainApp = (g: compiled, head: L.term, args: array<L.term>, localIdx: int, scope: array<L.meta>, ctx: printCtx): O.out => {
    let headOut = prettyPrintAt(g, head, localIdx, scope, AppFunc)
    let full = O.spaced(Array.concat([headOut], args->Array.map(a => prettyPrintAt(g, a, localIdx, scope, AppArg))))
    needsParens(g, AnApp, ctx) ? O.parens(full) : full
  }
  and printOpApplication = (g: compiled, op: opDecl, allArgs: array<L.term>, localIdx: int, scope: array<L.meta>, ctx: printCtx): O.out => {
    let n = holeCount(op)
    let opArgs = Array.slice(allArgs, ~start=0, ~end=n)
    let extra = Array.sliceToEnd(allArgs, ~start=n)
    let holeIdx = ref(0)
    let pieces = op.parts->Array.map(part =>
      switch part {
      | Lit(s) => O.leaf(~kind="op-lit", s)
      | Hole(_) => {
          let i = holeIdx.contents
          holeIdx := i + 1
          let pos = effectiveHolePos(op.assoc, i, n)
          prettyPrintAt(g, opArgs->Belt.Array.getExn(i), localIdx, scope, InCategory(op.category, pos))
        }
      }
    )
    let opOut = O.spaced(pieces)
    let shapeHere = Array.length(extra) > 0 ? AppFunc : ctx
    let opOut = needsParens(g, AnOp(op.category), shapeHere) ? O.parens(opOut) : opOut
    if Array.length(extra) == 0 {
      opOut
    } else {
      let full = O.spaced(Array.concat([opOut], extra->Array.map(a => prettyPrintAt(g, a, localIdx, scope, AppArg))))
      needsParens(g, AnApp, ctx) ? O.parens(full) : full
    }
  }

  let prettyPrintWithGrammar = (term: L.term, ~parentheses: bool, ~grammar: compiled, ~scope: array<L.meta>): O.out =>
    prettyPrintAt(grammar, term, 0, scope, parentheses ? AppArg : Top)
}

module StringTarget: PRINT_TARGET with type out = string = {
  type out = string
  let leaf = (~kind as _, s) => s
  let seq = arr => arr->Array.join("")
  let spaced = arr => arr->Array.join(" ")
  let parens = s => `(${s})`
}