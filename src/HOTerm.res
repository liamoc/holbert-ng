module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

module type S = {
  module Atom: AtomDef.ATOM_CHOICE
  type rec t =
    | Symbol({name: Atom.t, constructor: bool})
    | Var({idx: int})
    | Schematic({schematic: int})
    | Lam({name: string, body: t})
    | App({func: t, arg: t})
    | Unallowed

  include Signatures.TERM
    with type t := t
    and type meta = string
    and type schematic = int
    and type subst = Belt.Map.Int.t<t>

  let emptySubst: subst
  let strip: t => (t, array<t>)
  let app: (t, array<t>) => t
  let mkvars: int => array<t>
  let mapTerms: (t, t => t) => t
  exception UnifyFail(string)
  let substAdd: (subst, schematic, t) => subst
  let unifyTerm: (t, t, subst, ~gen: option<gen>) => Seq.t<subst>
  let reduceSubst: subst => subst
  let rewrite: (t, t, t, ~subst: subst, ~gen: option<gen>) => (subst, t)
  let isEqualityAtom: Atom.t => bool
}

module Make = (Atom: AtomDef.ATOM_CHOICE): (S with module Atom = Atom) => {
  module Atom = Atom
  type rec t =
    | Symbol({name: Atom.t, constructor: bool})
    | Var({idx: int})
    | Schematic({schematic: int})
    | Lam({name: string, body: t})
    | App({func: t, arg: t})
    // Unallowed is used internally in unify, where Nipkow 1993 uses Var(-infinity)
    | Unallowed
  type meta = string
  type schematic = int
  type subst = Belt.Map.Int.t<t>
  let substHas = (subst: subst, schematic: schematic) => subst->Belt.Map.Int.has(schematic)
  let substGet = (subst: subst, schematic: schematic) => subst->Belt.Map.Int.get(schematic)
  let mapSubst = (m: subst, f: t => t): subst => {
    m->Belt.Map.Int.map(f)
  }
  let substEqual = (m1, m2) => m1 == m2
  let makeSubst = () => {
    Belt.Map.Int.empty
  }
  let mergeSubsts = (m1: subst, m2: subst) =>
    Belt.Map.Int.merge(m1, m2, (_, o1, o2) =>
      switch (o1, o2) {
      | (Some(v), _) | (_, Some(v)) => Some(v)
      | (None, None) => None
      }
    )
  let rec equivalent = (a: t, b: t) => {
    switch (a, b) {
    | (Symbol({name: a}), Symbol({name: b})) => a == b
    | (Var({idx: ia}), Var({idx: ib})) => ia == ib
    | (Schematic({schematic: sa}), Schematic({schematic: sb})) => sa == sb
    | (Lam({name: _, body: ba}), Lam({name: _, body: bb})) => equivalent(ba, bb)
    | (App({func: fa, arg: aa}), App({func: fb, arg: ab})) =>
      equivalent(fa, fb) && equivalent(aa, ab)
    | (Unallowed, Unallowed) => false
    | (_, _) => false
    }
  }
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
  let rec schematicsIn = (subst: subst, it: t): Belt.Set.t<schematic, IntCmp.identity> =>
    switch it {
    | Schematic({schematic, _}) if subst->substHas(schematic) =>
      let found = subst->substGet(schematic)->Option.getExn
      schematicsIn(subst, found)
    | Schematic({schematic, _}) => Belt.Set.make(~id=module(IntCmp))->Belt.Set.add(schematic)
    | Lam({body}) => schematicsIn(subst, body)
    | App({func, arg}) => Belt.Set.union(schematicsIn(subst, func), schematicsIn(subst, arg))
    | Unallowed | Symbol(_) | Var(_) => Belt.Set.make(~id=module(IntCmp))
    }
  let occ = (schematic: schematic, subst: subst, t: t): bool => {
    let set = schematicsIn(subst, t)
    set->Belt.Set.has(schematic)
  }
  let rec freeVarsIn = (subst: subst, it: t): Belt.Set.t<int, IntCmp.identity> =>
    switch it {
    | Schematic({schematic, _}) if subst->substHas(schematic) =>
      let found = subst->substGet(schematic)->Option.getExn
      freeVarsIn(subst, found)
    | Var({idx}) => Belt.Set.make(~id=module(IntCmp))->Belt.Set.add(idx)
    | Lam({name: _, body}) =>
      freeVarsIn(subst, body)
      ->Belt.Set.toArray
      ->Array.filterMap(v =>
        if v >= 1 {
          Some(v - 1)
        } else {
          None
        }
      )
      ->Belt.Set.fromArray(~id=module(IntCmp))
    | App({func, arg}) => Belt.Set.union(freeVarsIn(subst, func), freeVarsIn(subst, arg))
    | Unallowed | Symbol(_) | Schematic(_) => Belt.Set.make(~id=module(IntCmp))
    }
  let freeVarsContains = (term: t, subst: subst, idx: int): bool => {
    let set = freeVarsIn(subst, term)
    set->Belt.Set.has(idx)
  }
  // f might map an index to a new one (Ok(newIdx)) or to a term (Error(t) with t(from)).
  // Note that Ok and Error here are not used for success or failure; they are just two cases distinguishing between an index and a term.
  let rec _mapbind0 = (term: t, f: int => result<int, int => t>, ~from: int=0): t =>
    switch term {
    | Symbol(_) => term
    | Var({idx}) =>
      if idx >= from {
        switch f(idx - from) {
        | Ok(newIdx) =>
          let new = newIdx + from
          if new < 0 {
            throw(Util.Err("mapbind: negative index"))
          }
          Var({
            idx: new,
          })
        | Error(t) => t(from)
        }
      } else {
        term
      }
    | Schematic({schematic}) =>
      Schematic({
        schematic: schematic,
      })
    | Lam({name, body}) =>
      Lam({
        name,
        body: _mapbind0(body, f, ~from=from + 1),
      })
    | App({func, arg}) =>
      App({
        func: _mapbind0(func, f, ~from),
        arg: _mapbind0(arg, f, ~from),
      })
    | Unallowed => Unallowed
    }
  let rec upshift = (term: t, amount: int, ~from: int=0) =>
    switch term {
    | Symbol({name, constructor}) => Symbol({name: Atom.upshift(name, amount, ~from), constructor})
    | Var({idx}) =>
      Var({
        idx: if idx >= from {
          idx + amount
        } else {
          idx
        },
      })
    | Schematic({schematic}) => Schematic({schematic: schematic})
    | Lam({name, body}) => Lam({name, body: upshift(body, amount, ~from=from + 1)})
    | App({func, arg}) =>
      App({func: upshift(func, amount, ~from), arg: upshift(arg, amount, ~from)})
    | Unallowed => Unallowed
    }
  let downshift = (term: t, amount: int, ~from: int=1) => {
    if amount > from {
      throw(Util.Err("downshift amount must be less than from"))
    }
    upshift(term, -amount, ~from)
  }
  let lookup = (term: t, subst: array<(t, t)>): option<t> => {
    subst
    ->Array.find(((from, _)) => equivalent(term, from))
    ->Option.map(((_, to)) => to)
  }
  let upshift_tt = (subst: array<(t, t)>, ~amount: int=1): array<(t, t)> => {
    subst->Array.map(((a, b)) => (upshift(a, amount), upshift(b, amount)))
  }
  // where pattern unification used mapbind we will need to use discharge for FCU
  //
  // When `prune` is true, it marks “dead” variables as Unallowed. Nipkow 1993 uses Var(-infinity) for this in the de Bruijn’s notation implementation.
  // Nipkow 1993's non de Bruijn implementation handle this logic in `proj`. Similarly Makoto Hamana's paper uses `elem` and `subst` in `prune`
  let rec discharge = (subst: array<(t, t)>, term: t, ~prune: bool): t => {
    switch lookup(term, subst) {
    | Some(found) => found
    | None =>
      switch term {
      | App({func, arg}) =>
        App({func: discharge(subst, func, ~prune), arg: discharge(subst, arg, ~prune)})
      // Lam case is not actually needed by FCU
      | Lam({name, body}) => Lam({name, body: discharge(upshift_tt(subst), body, ~prune)})
      | Var(_) if prune => Unallowed
      | Var(_) | Schematic(_) | Symbol(_) | Unallowed => term
      }
    }
  }
  let emptySubst: subst = Belt.Map.Int.empty
  let substAdd = (subst: subst, schematic: schematic, term: t) => {
    assert(schematic >= 0)
    assert(subst->Belt.Map.Int.has(schematic) == false)
    subst->Belt.Map.Int.set(schematic, term)
  }
  let rec strip = (term: t): (t, array<t>) => {
    switch term {
    | App({func, arg}) =>
      let (peeledFunc, peeledArgs) = strip(func)
      (peeledFunc, Array.concat(peeledArgs, [arg]))
    | _ => (term, [])
    }
  }
  let lower = (term: t): option<Atom.t> =>
    switch term {
    | Symbol(s) => Some(s.name)
    | Var({idx}) => Atom.coerce(AtomBase.VarBase.wrap(Var({idx: idx})))
    | _ =>
      switch strip(term) {
      | (Schematic({schematic}), ts) =>
        ts
        ->Array.map(t =>
          switch t {
          | Var({idx}) => Some(idx)
          | _ => None
          }
        )
        ->Util.Option.sequence
        ->Option.flatMap(allowed =>
          Atom.coerce(AtomBase.VarBase.wrap(Schematic({schematic, allowed})))
        )
      | _ => None
      }
    }
  let rec substitute = (term: t, subst: subst) =>
    switch term {
    | Schematic({schematic, _}) =>
      switch Belt.Map.Int.get(subst, schematic) {
      | None => term
      | Some(found) => found
      }
    | Lam({name, body}) =>
      Lam({
        name,
        // upshift is not needed for pattern unification, but it is safer to have upshift here
        body: substitute(body, subst->Belt.Map.Int.map(t => upshift(t, 1))),
      })
    | App({func, arg}) =>
      App({
        func: substitute(func, subst),
        arg: substitute(arg, subst),
      })
    | Symbol({name, constructor}) => {
        let symbolSubst = Map.make()
        subst->Belt.Map.Int.forEach((k, v) => {
          lower(v)->Option.map(v => Map.set(symbolSubst, k, v))->ignore
        })
        Symbol({name: Atom.substitute(name, symbolSubst), constructor})
      }
    | Var(_) | Unallowed => term
    }

  let rec substDeBruijn = (term: t, substs: array<t>, ~from: int=0) =>
    switch term {
    | Symbol({name, constructor}) =>
      Symbol({
        name: Atom.substDeBruijn(name, substs->Array.map(lower), ~from),
        constructor,
      })
    | Var({idx: var}) =>
      if var < from {
        term
      } else if var - from < Array.length(substs) && var - from >= 0 {
        Option.getExn(substs[var - from])
      } else {
        Var({idx: var - Array.length(substs)})
      }
    | Schematic({schematic}) =>
      Schematic({
        schematic: schematic,
      })
    | Lam({name, body}) =>
      Lam({
        name,
        body: substDeBruijn(body, substs->Array.map(term => upshift(term, 1)), ~from=from + 1),
      })
    | App({func, arg}) =>
      App({
        func: substDeBruijn(func, substs, ~from),
        arg: substDeBruijn(arg, substs, ~from),
      })
    | Unallowed => Unallowed
    }
  // beta reduced and eta reduced
  let rec reduce = (term: t): t => {
    switch term {
    | Lam({body: App({func, arg: Var({idx: 0})})}) if !(func->freeVarsContains(emptySubst, 0)) =>
      reduce(downshift(func, 1))
    | App({func, arg}) =>
      switch reduce(func) {
      | Lam({body}) => reduce(substDeBruijn(body, [arg]))
      | func => App({func, arg: reduce(arg)})
      }
    | Lam({name, body}) =>
      Lam({
        name,
        body: reduce(body),
      })
    | Symbol(_) | Var(_) | Schematic(_) => term

    | Unallowed => Unallowed
    }
  }
  let reduceSubst = (subst: subst): subst => {
    subst->Belt.Map.Int.map(x => reduce(substitute(x, subst)))
  }
  let rec lams = (amount: int, term: t): t => {
    assert(amount >= 0)
    if amount <= 0 {
      term
    } else {
      Lam({
        name: "x",
        body: lams(amount - 1, term),
      })
    }
  }
  let rec idx = (is: array<t>, j: t): option<int> => {
    if is->Array.length == 0 {
      None
    } else {
      let head = is[0]->Option.getExn
      let tail = is->Array.sliceToEnd(~start=1)
      if equivalent(head, j) {
        Some(tail->Array.length)
      } else {
        idx(tail, j)
      }
    }
  }
  let idx1' = (is: array<t>, j: t): t => {
    switch idx(is, j) {
    | None => Unallowed
    | Some(idx) => Var({idx: idx})
    }
  }
  let _idx1 = (is: array<t>, j: int): t => idx1'(is, Var({idx: j}))
  let idx2' = (is: array<t>, j: t): result<int, int => t> => {
    switch idx(is, j) {
    | None => Error(_ => Unallowed)
    | Some(idx) => Ok(idx)
    }
  }
  let _idx2 = (is: array<t>, j: int) => idx2'(is, Var({idx: j}))
  let rec app = (term: t, args: array<t>): t => {
    if args->Array.length == 0 {
      term
    } else {
      let head = args[0]->Option.getExn
      let rest = args->Array.sliceToEnd(~start=1)
      app(App({func: term, arg: head}), rest)
    }
  }
  exception UnifyFail(string)
  let rec red = (term: t, is: array<t>): t => {
    switch term {
    | _ if is->Array.length == 0 => term
    | Lam({body}) =>
      red(substDeBruijn(body, [is[0]->Option.getExn]), is->Array.sliceToEnd(~start=1))
    | term => app(term, is)
    }
  }
  let lam = (is: array<t>, g: t, js: array<t>): t => {
    lams(is->Array.length, app(g, js->Array.map(j => idx1'(is, j))))
  }
  let rec devar = (subst: subst, term: t): t => {
    let (func, args) = strip(term)
    switch func {
    | Schematic({schematic}) if substHas(subst, schematic) =>
      devar(subst, red(substGet(subst, schematic)->Option.getExn, args))
    | _ => term
    }
  }
  let mkvars = (n: int): array<t> => {
    Belt.Array.init(n, i => n - i - 1)->Array.map(x => Var({idx: x}))
  }
  let rec proj_allowed = (subst: subst, term: t): bool => {
    let term' = devar(subst, term)
    switch term' {
    | Lam(_) | Unallowed | Schematic(_) | Symbol(_) => false
    | Var(_) => true // pattern unification only allows this
    // FCU allows this:
    | App(_) =>
      switch strip(term') {
      | (Symbol(_) | Var(_), args) => Array.every(args, x => proj_allowed(subst, x))
      | _ => false
      }
    }
  }
  // this function is called proj in Nipkow 1993 and it is called pruning in FCU paper
  let rec proj = (subst: subst, term: t, ~gen: option<gen>): subst => {
    switch strip(devar(subst, term)) {
    | (Lam({name: _, body}), args) if args->Array.length == 0 => proj(subst, body, ~gen)
    | (Unallowed, _args) => throw(UnifyFail("unallowed"))
    | (Symbol(_) | Var(_), args) => Array.reduce(args, subst, (acc, a) => proj(acc, a, ~gen))
    | (Schematic({schematic}), args) => {
        assert(!substHas(subst, schematic))
        if gen->Option.isNone {
          throw(UnifyFail("no gen provided"))
        }
        let h = Schematic({schematic: fresh(Option.getExn(gen))})
        subst->substAdd(
          schematic,
          lams(
            args->Array.length,
            app(
              h,
              Belt.Array.init(args->Array.length, j => {
                if proj_allowed(subst, args[j]->Option.getExn) {
                  Some(Var({idx: args->Belt.Array.length - j - 1}))
                } else {
                  None
                }
              })->Array.keepSome,
            ),
          ),
        )
      }
    | _ => throw(UnifyFail("not a symbol, var or schematic"))
    }
  }
  let flexflex = (
    sa: schematic,
    xs: array<t>,
    sb: schematic,
    ys: array<t>,
    subst: subst,
    ~gen: option<gen>,
  ): subst => {
    if gen->Option.isNone {
      throw(UnifyFail("no gen provided"))
    }
    if sa == sb {
      if xs->Array.length != ys->Array.length {
        throw(UnifyFail("flexible schematics have different number of arguments"))
      }
      let len = xs->Array.length
      let h = Schematic({schematic: fresh(Option.getExn(gen))})
      let xs = Belt.Array.init(len, k => {
        let a = xs[k]->Option.getExn
        let b = ys[k]->Option.getExn
        if equivalent(a, b) {
          Some(Var({idx: len - k - 1}))
        } else {
          None
        }
      })->Array.keepSome
      subst->substAdd(sa, lams(len, app(h, xs)))
    } else {
      let common = xs->Array.filter(x => ys->Belt.Array.some(y => equivalent(x, y)))
      let h = Schematic({schematic: fresh(Option.getExn(gen))})
      subst->substAdd(sa, lam(xs, h, common))->substAdd(sb, lam(ys, h, common))
    }
  }
  let flexrigid = (sa: schematic, xs: array<t>, b: t, subst: subst, ~gen: option<gen>): subst => {
    if occ(sa, subst, b) {
      throw(UnifyFail("flexible schematic occurs in rigid term"))
    }
    // pattern unification
    // let u = b->_mapbind0(bind => idx2(xs, bind))
    // FCU
    let zn = mkvars(xs->Array.length)
    // we reversed it so that the last one will be picked if there are duplicates. This behaviour helps certain real world cases.
    let u = discharge(Belt.Array.reverse(Belt.Array.zip(xs, zn)), b, ~prune=true)
    proj(subst->substAdd(sa, lams(xs->Array.length, u)), u, ~gen)
  }
  let rec integrateAtomSubst = (atomSubst: Atom.subst, subst: subst, ~gen: option<gen>): subst =>
    atomSubst
    ->Map.entries
    ->Iterator.toArray
    ->Array.reduce(subst, (acc, (schematic, term)) =>
      if acc->substHas(schematic) {
        unifyTermFirst(
          acc->substGet(schematic)->Option.getExn,
          Symbol({name: term, constructor: false}),
          acc,
          ~gen,
        )
      } else {
        acc->substAdd(schematic, Symbol({name: term, constructor: false}))
      }
    )
  and unifyAtom = (a: Atom.t, b: Atom.t, subst: subst, ~gen: option<gen>): Seq.t<subst> =>
    Atom.unify(a, b)->Seq.filterMap(atomSubst =>
      try {
        Some(integrateAtomSubst(atomSubst, subst, ~gen))
      } catch {
      | UnifyFail(_) => None
      }
    )
  and unifyTerm = (a: t, b: t, subst: subst, ~gen: option<gen>): Seq.t<subst> =>
    switch (devar(subst, a), devar(subst, b)) {
    | (Symbol({name: a}), Symbol({name: b})) => unifyAtom(a, b, subst, ~gen)
    | (Var({idx: ia}), Var({idx: ib})) =>
      if ia == ib {
        Seq.once(subst)
      } else {
        Seq.empty
      }
    | (Schematic({schematic: sa}), Schematic({schematic: sb})) if sa == sb => Seq.once(subst)
    | (Lam({name: _, body: ba}), Lam({name: _, body: bb})) => unifyTerm(ba, bb, subst, ~gen)
    | (Lam({name: _, body: ba}), b) =>
      unifyTerm(ba, App({func: upshift(b, 1), arg: Var({idx: 0})}), subst, ~gen)
    | (a, Lam({name: _, body: bb})) =>
      unifyTerm(App({func: upshift(a, 1), arg: Var({idx: 0})}), bb, subst, ~gen)
    | (a, b) =>
      switch (strip(a), strip(b)) {
      | ((Schematic({schematic: sa}), xs), (Schematic({schematic: sb}), ys)) =>
        try {
          Seq.once(flexflex(sa, xs, sb, ys, subst, ~gen))
        } catch {
        | UnifyFail(_) => Seq.empty
        }
      | ((Schematic({schematic: sa}), xs), _) =>
        try {
          Seq.once(flexrigid(sa, xs, b, subst, ~gen))
        } catch {
        | UnifyFail(_) => Seq.empty
        }
      | (_, (Schematic({schematic: sb}), ys)) =>
        try {
          Seq.once(flexrigid(sb, ys, a, subst, ~gen))
        } catch {
        | UnifyFail(_) => Seq.empty
        }
      | ((a, xs), (b, ys)) =>
        switch (a, b) {
        | (Symbol(_), Symbol(_)) | (Var(_), Var(_)) =>
          unifyTerm(a, b, subst, ~gen)->Seq.flatMap(subst => unifyArray(xs, ys, subst, ~gen))
        | _ => Seq.empty
        }
      }
    }
  and unifyArray = (xs: array<t>, ys: array<t>, subst: subst, ~gen: option<gen>): Seq.t<subst> => {
    if xs->Array.length != ys->Array.length {
      Seq.empty
    } else if xs->Array.length == 0 {
      Seq.once(subst)
    } else {
      let x = xs[0]->Option.getExn
      let y = ys[0]->Option.getExn
      let restXs = xs->Array.sliceToEnd(~start=1)
      let restYs = ys->Array.sliceToEnd(~start=1)
      unifyTerm(x, y, subst, ~gen)->Seq.flatMap(subst => unifyArray(restXs, restYs, subst, ~gen))
    }
  }
  and unifyTermFirst = (a: t, b: t, subst: subst, ~gen: option<gen>): subst =>
    switch unifyTerm(a, b, subst, ~gen)->Seq.head {
    | Some(subst) => subst
    | None =>
      switch (devar(subst, a), devar(subst, b)) {
      | (Symbol(_), Symbol(_)) => throw(UnifyFail("symbols do not match"))
      | (Var(_), Var(_)) => throw(UnifyFail("variables do not match"))
      | _ => throw(UnifyFail("no rules match"))
      }
    }
  let unify = (a: t, b: t, ~gen=?) => unifyTerm(a, b, emptySubst, ~gen)
  let rec rewrite = (term: t, from: t, to: t, ~subst: subst, ~gen: option<gen>): (subst, t) => {
    try {
      let subst1 = unifyTermFirst(term, from, subst, ~gen)
      (subst1, to)
    } catch {
    | UnifyFail(_) =>
      switch term {
      | Schematic({schematic}) if subst->substHas(schematic) =>
        rewrite(subst->substGet(schematic)->Option.getExn, from, to, ~subst, ~gen)
      | Var(_) | Unallowed | Symbol(_) | Schematic(_) => (subst, term)
      | Lam({name, body}) => {
          let (subst1, body1) = rewrite(body, from, to, ~subst, ~gen)
          (subst1, Lam({name, body: body1}))
        }
      | App({func, arg}) => {
          let (subst1, func') = rewrite(func, from, to, ~subst, ~gen)
          let (subst2, arg') = rewrite(arg, from, to, ~subst=subst1, ~gen)
          (subst2, App({func: func', arg: arg'}))
        }
      }
    }
  }
  let place = (x: int, ~scope: array<string>) =>
    app(
      Schematic({
        schematic: x,
      }),
      Array.fromInitializer(~length=Array.length(scope), i => Var({idx: i})),
    )

  let isEqualityAtom = (AtomBase.AnyValue(tag, v): Atom.t): bool =>
    switch tag {
    | Symbolic.Base.Tag => v == "="
    | _ => false
    }
  let prettyPrintVar = (idx: int, scope: array<string>) =>
    switch scope[idx] {
    | Some(n) if Array.indexOf(scope, n) == idx => n
    | _ => "\\"->String.concat(String.make(idx))
    }
  let makeGen = () => {
    ref(0)
  }
  let rec stripLam = (it: t): (array<string>, t) =>
    switch it {
    | Lam({name, body}) =>
      let (names, body) = stripLam(body)
      (Array.concat([name], names), body)
    | _ => ([], it)
    }
  let rec prettyPrint = (it: t, ~scope: array<string>) =>
    switch it {
    | Symbol({name, constructor}) =>
      if constructor {
        String.concat("@", Atom.prettyPrint(name, ~scope))
      } else {
        Atom.prettyPrint(name, ~scope)
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
    | Unallowed => ""
    }
  let prettyPrintSubst = (sub: subst, ~scope: array<string>) =>
    Util.prettyPrintIntMap(sub, ~showV=t => prettyPrint(t, ~scope))
  let nameRES = "^([^\\s.\\[\\]()]+)\\."
  exception ParseError(string)
  type token =
    | LParen
    | RParen
    | VarT(int)
    | SchematicT(int)
    | AtomT(Atom.t)
    | ConsT(Atom.t)
    | NameT(string)
    | EOF
  let varRegexpString = "^\\\\([0-9]+)"
  let schematicRegexpString = "^\\?([0-9]+)"
  let atomToken = (str: string, ~scope: array<string>, ~gen=?) => {
    switch Atom.parse(str, ~scope, ~gen?) {
    | Ok((atom, rest)) => (atom, rest)
    | Error(msg) => throw(ParseError(msg))
    }
  }
  let scopeVarToken = (str: string, scope: array<string>): option<(int, string)> => {
    let result = ref(None)
    scope->Array.forEachWithIndex((name, idx) => {
      let len = String.length(name)
      let matches =
        String.slice(str, ~start=0, ~end=len) == name &&
          switch str->String.charAt(len) {
          | "" | " " | "\t" | "\n" | "\r" | "(" | ")" => true
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
      | "@" =>
        let (raw, rest) = atomToken(rest(), ~scope, ~gen?)
        (ConsT(raw), rest)
      | _ => {
          let reName = RegExp.fromStringWithFlags(nameRES, ~flags="y")
          switch reName->RegExp.exec(str) {
          | Some(res) =>
            switch RegExp.Result.matches(res) {
            | [n] => (NameT(n), String.sliceToEnd(str, ~start=RegExp.lastIndex(reName)))
            | _ => throw(ParseError("invalid symbol"))
            }
          | None =>
            switch scopeVarToken(str, scope) {
            | Some((idx, rest)) => (VarT(idx), rest)
            | None =>
              let (atom, rest) = atomToken(str, ~scope, ~gen?)
              (AtomT(atom), rest)
            }
          }
        }
      }
    }
  }
  type rec simple =
    | ListS({xs: array<simple>})
    | AtomS({name: Atom.t, constructor: bool})
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

  let concrete = t =>
    switch t {
    | Schematic(_) => false
    | Symbol({name}) => Atom.concrete(name)
    | _ => true
    }
  let mapTerms = (t, f) => f(t)
}
