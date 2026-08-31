open MixfixGrammar

module type PARSE_LEAF = {
  type term
  type meta
  type gen
  let parseLeaf: (
    string,
    ~reserved: Belt.Set.String.t,
    ~scope: array<meta>,
    ~gen: gen=?,
    ~recur: (string, ~scope: array<meta>, ~gen: gen=?) => result<(term, string), string>,
  ) => result<(term, string), string>
  let mkApp: (term, term) => term
  let mkOpHead: (~name: string) => term
}

module Make = (L: PARSE_LEAF) => {
  let rec parseAtom = (
    g: compiled,
    input: string,
    ~scope: array<L.meta>,
    ~gen=?,
  ): result<(L.term, string), string> =>
    L.parseLeaf(
      input,
      ~reserved=g.reserved,
      ~scope,
      ~gen?,
      ~recur=(i, ~scope, ~gen=?) => parseTop(g, i, ~scope, ~gen?),
    )
  and parseApp = (g: compiled, input: string, ~scope, ~gen=?): result<(L.term, string), string> =>
    switch parseAtom(g, input, ~scope, ~gen?) {
    | Error(e) => Error(e)
    | Ok((head, rest)) => appLoop(g, head, rest, ~scope, ~gen?)
    }
  and appLoop = (g: compiled, left: L.term, input: string, ~scope, ~gen=?): result<(L.term, string), string> =>
    switch parseAtom(g, input, ~scope, ~gen?) {
    | Ok((arg, rest)) => appLoop(g, L.mkApp(left, arg), rest, ~scope, ~gen?)
    | Error(_) => Ok((left, input))
    }
  and parseCategory = (g: compiled, cat: string, input: string, ~scope, ~gen=?): result<(L.term, string), string> =>
    switch parseOperand(g, cat, input, ~scope, ~gen?) {
    | Error(e) => Error(e)
    | Ok((left, rest)) => infixLoop(g, cat, left, rest, ~scope, ~gen?)
    }
  and parseOperand = (g: compiled, cat: string, input: string, ~scope, ~gen=?): result<(L.term, string), string> => {
    let prefixOps =
      g.ops->Array.filter(op =>
        op.category == cat &&
          (op.parts[0]
          ->Option.map(p => switch p { | Lit(_) => true | Hole(_) => false })
          ->Option.getOr(false))
      )
    switch tryOps(g, prefixOps, None, input, ~scope, ~gen?) {
    | Some(Ok(_) as ok) => ok
    | Some(Error(_) as e) => e
    | None =>
      switch g.immediateTighter->Dict.get(cat) {
      | Some(tighters) =>
        let rec tryAll = idx => {
          switch Belt.Array.get(tighters, idx) {
          | None => Error(`expected a term (category ${cat})`)
          | Some(t') =>
            switch parseCategory(g, t', input, ~scope, ~gen?) {
            | Ok(_) as ok => ok
            | Error(_) => tryAll(idx + 1)
            }
          }
        }
        tryAll(0)
      | None => parseApp(g, input, ~scope, ~gen?)
      }
    }
  }
  and infixLoop = (
    g: compiled,
    cat: string,
    left: L.term,
    input: string,
    ~scope,
    ~gen=?,
  ): result<(L.term, string), string> => {
    let infixOps =
      g.ops->Array.filter(op =>
        op.category == cat &&
          (op.parts[0]
          ->Option.map(p => switch p { | Hole(_) => true | Lit(_) => false })
          ->Option.getOr(false))
      )
    switch tryOps(g, infixOps, Some(left), input, ~scope, ~gen?) {
    | Some(Ok((combined, rest))) =>
      // NOTE: checks whether ANY op is NonAssoc
      let anyNonAssoc = infixOps->Array.some(op => op.assoc == NonAssoc)
      anyNonAssoc ? Ok((combined, rest)) : infixLoop(g, cat, combined, rest, ~scope, ~gen?)
    | Some(Error(_) as e) => e
    | None => Ok((left, input))
    }
  }
  and tryOps = (
    g: compiled,
    ops: array<opDecl>,
    left: option<L.term>,
    input: string,
    ~scope,
    ~gen=?,
  ): option<result<(L.term, string), string>> => {
    let startIdx = left->Option.isSome ? 1 : 0
    let argsAcc = left->Option.map(l => [l])->Option.getOr([])

    let rec go = idx =>
      switch Belt.Array.get(ops, idx) {
      | None => None
      | Some(op) =>
        switch matchOp(g, op, startIdx, startIdx, input, argsAcc, ~scope, ~gen?) {
        | None => go(idx + 1)
        | Some(Error(_) as e) => Some(e)
        | Some(Ok((args, remaining))) =>
          let head = L.mkOpHead(~name=op.name)
          Some(Ok((args->Array.reduce(head, (f, a) => L.mkApp(f, a)), remaining)))
        }
      }

    go(0)
  }
  and matchOp = (
    g: compiled,
    op: opDecl,
    i: int,
    startIdx: int,
    input: string,
    argsAcc: array<L.term>,
    ~scope,
    ~gen=?,
  ): option<result<(array<L.term>, string), string>> =>
    switch op.parts->Belt.Array.get(i) {
    | None => Some(Ok((argsAcc, input)))
    | Some(Lit(s)) =>
      let input = MixfixLex.skipWs(input)
      switch MixfixLex.takeIdent(input) {
      | Some((tok, rest)) if tok == s => matchOp(g, op, i + 1, startIdx, rest, argsAcc, ~scope, ~gen?)
      | _ => i == startIdx ? None : Some(Error(`expected "${s}"`))
      }
    | Some(Hole(pos)) =>
      let isLast = i == Array.length(op.parts) - 1
      let sub =
        pos == Self && isLast && op.assoc == Right
          ? parseCategory(g, op.category, input, ~scope, ~gen?)
          : parseOperand(g, op.category, input, ~scope, ~gen?)
      switch sub {
      | Error(e) => Some(Error(e))
      | Ok((arg, rest)) => matchOp(g, op, i + 1, startIdx, rest, Array.concat(argsAcc, [arg]), ~scope, ~gen?)
      }
    }    
  and parseTop = (g: compiled, input: string, ~scope, ~gen=?): result<(L.term, string), string> => {
    let attempts =
      g.roots->Array.filterMap(r =>
        switch parseCategory(g, r, input, ~scope, ~gen?) {
        | Ok((_, rest)) as ok => Some((ok, String.length(rest)))
        | Error(_) => None
        }
      )
    let fallback = parseApp(g, input, ~scope, ~gen?)
    switch attempts {
    | [] => fallback
    | _ =>
      // prefer whichever attempt left the LEAST leftover (consumed the
      // most input) — ties broken by first-declared, same as before
      let (best, _) =
        attempts->Array.reduce(attempts->Belt.Array.getExn(0), ((bestOk, bestLen), (ok, len)) =>
          len < bestLen ? (ok, len) : (bestOk, bestLen)
        )
      best
    }
  }

  let parse = (input: string, ~grammar: compiled, ~scope: array<L.meta>, ~gen=?): result<(L.term, string), string> =>
    parseAtom(grammar, input, ~scope, ~gen?)
}