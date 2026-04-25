module type AC_ATOM = {
  module ACBase: AssocCommBase.AC_BASE_ATOM
  include AtomDef.ATOM with module Base = ACBase
  let identity: t
  let add: (t, t) => t
}

module Make = (
  BaseAtom: AssocCommBase.AC_BASE_ATOM,
  C: {
    let coerce: AtomBase.anyValue => option<BaseAtom.t>
  },
): (AC_ATOM with module ACBase = BaseAtom) => {
  module ACBase = BaseAtom
  module Base = BaseAtom
  module Const = BaseAtom.Const
  module IntMap = Belt.Map.Int
  type t = Base.t
  type subst = Map.t<int, t>
  type gen = ref<int>
  let identity = Base.identity
  let add = (a1: t, a2: t) => {
    Base.schemas: IntMap.merge(a1.schemas, a2.schemas, (_, o1, o2) =>
      switch (o1, o2) {
      | (Some({allowed, count}), None) | (None, Some({allowed, count})) =>
        Some({Base.allowed, count})
      | (Some({allowed, count: count1}), Some({count: count2})) =>
        Some({allowed, count: count1 + count2})
      | (None, None) => throw(Util.Unreachable("belt map error"))
      }
    ),
    vars: IntMap.merge(a1.vars, a2.vars, (_, o1, o2) =>
      switch (o1, o2) {
      | (Some(count), None) | (None, Some(count)) => Some(count)
      | (Some(count1), Some(count2)) => Some(count1 + count2)
      | (None, None) => throw(Util.Unreachable("belt map error"))
      }
    ),
    const: a1.const + a2.const,
  }
  let multiply = (a: t, n: int) => {
    Base.schemas: a.schemas->IntMap.map(({allowed, count}) => {Base.allowed, count: count * n}),
    vars: a.vars->IntMap.map(count => count * n),
    const: a.const * n,
  }
  let singletonSubst: (int, t) => subst = (schematic, term) => {
    let subst = Map.make()
    subst->Map.set(schematic, term)
    subst
  }
  let prettyPrint = (t: t, ~scope: array<string>) => {
    open Const
    let opString = ` ${opString} `
    let schemas = switch t.schemas->IntMap.size {
    | 0 => None
    | _ =>
      t.schemas
      ->IntMap.toArray
      ->Array.map(((schematic, {allowed, count})) =>
        Util.prettyPrintSchematic(schematic, allowed, scope)->formatMultiple(count)
      )
      ->Array.join(opString)
      ->Some
    }

    let vars = switch t.vars->IntMap.size {
    | 0 => None
    | _ =>
      t.vars
      ->IntMap.toArray
      ->Array.map(((idx, count)) => Util.prettyPrintVar(idx, scope)->formatMultiple(count))
      ->Array.join(opString)
      ->Some
    }
    let const = switch t.const {
    | 0 => None
    | _ => t.const->formatMultipleConst->Some
    }
    let inner = [schemas, vars, const]->Array.keepSome->Array.join(opString)
    `${openTerm} ${inner} ${closeTerm}`
  }
  // {0, 1, ..., n - 1}^m
  let latticePoints = (n: int, m: int): Seq.t<array<int>> =>
    Seq.init(n ** m, i => {
      let vec = []
      let x = ref(i)
      for _ in 0 to m - 1 {
        vec->Array.push(x.contents % n)
        x := x.contents / n
      }
      vec
    })
  let compWiseGt = (v1: array<int>, v2: array<int>): bool =>
    Belt.Array.zip(v1, v2)->Array.every(((x, y)) => x > y)
  let dot = (v1: array<int>, v2: array<int>): int =>
    Belt.Array.zipBy(v1, v2, (a, b) => a * b)->Array.reduce(0, (a, b) => a + b)
  let unify = (a: t, b: t, ~gen as _=?) => {
    let solveNaive = (a: t, b: t) => {
      let schema = a.schemas->IntMap.minKey->Option.getExn
      Base.Const.opInv(b.const, a.const)
      ->Option.map(n => Seq.once(singletonSubst(schema, Base.const(n))))
      ->Option.getOr(Seq.empty)
    }
    let isNaiveCase = (a: t, b: t): bool => {
      a.schemas->IntMap.size == 1 &&
      b.schemas->IntMap.size == 0 &&
      a.vars->IntMap.size == 0 &&
      b.vars->IntMap.size == 0 &&
      (a.schemas->IntMap.get(a.schemas->IntMap.minKey->Option.getExn)->Option.getExn).count == 1
    }

    // steps in elementary algorithm
    // 1. gen schema vector `schemas` by lhs - rhs. this has length n
    // 2. test all vectors in {0, 1, ..., max(schemas) + 1}^n for eq to 0
    //    vectors that match should be filtered as we go for being (component-wise) minimal
    // 3. reconstruct sub: for each v_i from previous step,
    //    schema_i |-> sum(v_j[i] * schema_j for j in 0..n)
    //
    // we /should/ be able to extend this to elemtary w/ constants
    // bc that just requires an inhomogeneous eqn solver, but i'm nto so
    // sure about the vectors above giving a minimal compelte set of sols
    let homogeneousSolve = () => {
      let eqnLhs = a.schemas->IntMap.merge(b.schemas, (_, o1, o2) => {
        switch (o1, o2) {
        | (Some({allowed, count}), None) => Some({Base.allowed, count})
        | (None, Some({allowed, count})) => Some({Base.allowed, count: -count})
        | (Some({allowed, count: count1}), Some({count: count2})) =>
          Some({allowed, count: count1 - count2})
        | (None, None) => throw(Util.Unreachable("belt map error"))
        }
      })
      let eqnEntries = eqnLhs->IntMap.toArray
      let counts = eqnEntries->Array.map(((_, entry)) => entry.count)
      let maxCount = counts->Array.reduce(0, (curMax, next) => max(curMax, abs(next)))
      let minimalSols = []
      let addIfMinimal = (sols: array<array<int>>, candidate) =>
        if sols->Array.every(sol => !(candidate->compWiseGt(sol))) {
          sols->Array.push(candidate)
        }
      latticePoints(maxCount + 1, eqnEntries->Array.length)
      // skip [0, ..., 0] (trivial solution to homogeneous eqn)
      ->Seq.tail
      ->Seq.forEach(vec => {
        if dot(vec, counts) == 0 {
          minimalSols->addIfMinimal(vec)
        }
      })
      let solutionToSubst = (sol: array<int>, target: t): Map.t<int, t> => {
        eqnEntries
        ->Belt.Array.zipBy(sol, ((schematic, _), count) =>
          if count > 0 {
            Some((schematic, multiply(target, count)))
          } else {
            None
          }
        )
        ->Array.keepSome
        ->Map.fromArray
      }
      let res =
        minimalSols
        ->Belt.Array.mapWithIndex((i, sol) => {
          let target = switch Belt.Array.get(eqnEntries, i) {
          | Some((schematic, {allowed})) => Base.schematic(schematic, allowed)
          | None => identity
          }
          solutionToSubst(sol, target)
        })
        ->Array.reduce(Map.make(), (sig1, sig2) => Util.mapUnionWith(sig1, sig2, add))
      res->Seq.once
    }
    if a.schemas->IntMap.size == 0 && b.schemas->IntMap.size == 0 {
      Seq.once(Map.make())
    } else if isNaiveCase(a, b) {
      solveNaive(a, b)
    } else if isNaiveCase(b, a) {
      solveNaive(b, a)
    } else if a.const - b.const == 0 {
      homogeneousSolve()
    } else {
      Seq.empty
    }
  }

  let parse = (str, ~scope: array<string>, ~gen: option<gen>=?) => {
    open Parser
    let var =
      Const.parseVar(~scope)
      ->map(Base.var)
      ->label("var")
    let schemaLit =
      Const.parseSchema(~gen?, ~scope)
      ->map(((schematic, allowed)) => Base.schematic(schematic, allowed))
      ->label("schematic")
    let const =
      Const.parseConst
      ->map(Base.const)
      ->label("const")
    let factor = choice([var, schemaLit, const])->lexeme
    let term =
      factor->bind(f =>
        many(token(Const.opString)->then(factor))->map(fs => fs->Array.reduce(f, add))
      )
    let full = term->between(token(Const.openTerm), token(Const.closeTerm))
    Parser.runParser(full, str)
  }

  let substitute = (atom: t, subst: subst): t => {
    let substituted =
      atom.schemas
      ->IntMap.toArray
      ->Array.map(((key, {allowed, count})) =>
        switch Map.get(subst, key) {
        | None => multiply(Base.schematic(key, allowed), count)
        | Some(sub) => multiply(sub, count)
        }
      )
      ->Array.reduce(identity, add)
    substituted->add({...identity, vars: atom.vars, const: atom.const})
  }
  let substDeBruijn = (atom: t, substs: array<option<t>>, ~from: int=0): t => {
    let substituted =
      atom.vars
      ->IntMap.toArray
      ->Array.map(((idx, count)) =>
        if idx < from {
          multiply(Base.var(idx), count)
        } else if idx - from < Array.length(substs) {
          switch substs[idx - from]->Option.getExn {
          | Some(a) => multiply(a, count)
          | None =>
            throw(SExp.SubstNotCompatible(`index ${Int.toString(idx - from)} not of sort blah`))
          }
        } else {
          multiply(Base.var(idx - Array.length(substs)), count)
        }
      )
      ->Array.reduce(identity, add)
    let schemas = atom.schemas->IntMap.map(({count, allowed}) => {
      Base.count,
      allowed: allowed->Array.filterMap(i =>
        if i < from + Array.length(substs) {
          None
        } else {
          Some(i - (from + Array.length(substs)))
        }
      ),
    })
    substituted->add({...identity, schemas, const: atom.const})
  }
  let concrete = (atom: t) => atom.vars->IntMap.size > 0 || atom.const > 0

  let upshift = (atom: t, amount: int, ~from: int=0) => {
    let vars =
      atom.vars
      ->IntMap.toArray
      ->Array.map(((idx, count)) =>
        if idx >= from {
          (idx + amount, count)
        } else {
          (idx, count)
        }
      )
      ->IntMap.fromArray
    let schemas = atom.schemas->IntMap.map(({allowed, count}) => {
      Base.count,
      allowed: allowed->Array.map(i =>
        if i >= from {
          i + amount
        } else {
          i
        }
      ),
    })
    {Base.vars, schemas, const: atom.const}
  }
  let coerce = C.coerce
}

module MakeView = (
  Base: AssocCommBase.AC_BASE_ATOM,
  Atom: AtomDef.ATOM with module Base = Base,
) => {
  module Const = Base.Const
  type props = {atom: Atom.t, scope: array<string>}
  let renderIf = (component, cond) =>
    if cond {
      component
    } else {
      React.null
    }
  module Var = {
    type props = {idx: int, count?: int, scope: array<string>}
    let make = (props: props) => {
      let count =
        props.count
        ->Option.map(n => React.int(n)->renderIf(n > 1))
        ->Option.getOr(React.null)
      switch props.scope[props.idx] {
      | Some(n) if Array.indexOf(props.scope, n) == props.idx =>
        <span className="term-metavar">
          {count}
          {React.string(n)}
        </span>
      | _ =>
        <span className="term-metavar-unnamed">
          {count}
          {React.string("\\")}
          {React.int(props.idx)}
        </span>
      }
    }
  }

  let parenthesise = f =>
    React.array([
      <span className="symbol-nat" key={"-1"}> {React.string("(")} </span>,
      f,
      <span className="symbol-nat" key={"-2"}> {React.string(")")} </span>,
    ])

  let intersperseOp = (a: array<React.element>) =>
    Util.intersperse(a, ~with=React.string(Const.opString))->React.array
  let make = ({atom, scope}: props) => {
    let schemas = switch atom.schemas->Belt.Map.Int.size {
    | 0 => None
    | _ =>
      atom.schemas
      ->Belt.Map.Int.toArray
      ->Array.map(((schematic, {allowed, count})) =>
        <span className="term-schematic">
          {React.string(`${count->Int.toString}?`)}
          {React.int(schematic)}
          <span className="term-schematic-telescope">
            {allowed
            ->Array.mapWithIndex((idx, _) => <Var idx scope />)
            ->intersperseOp}
          </span>
        </span>
      )
      ->React.array
      ->Some
    }
    let vars = switch atom.vars->Belt.Map.Int.size {
    | 0 => None
    | _ =>
      atom.vars
      ->Belt.Map.Int.toArray
      ->Array.map(((idx, count)) => <Var idx scope count key={idx->Int.toString} />)
      ->intersperseOp
      ->Some
    }
    let const = switch (schemas, vars) {
    | (_, Some(_)) | (Some(_), _) if atom.const > 0 => Some(React.int(atom.const))
    | (None, None) => Some(React.int(atom.const))
    | _ => None
    }
    <span className="term-compound">
      {[schemas, vars, const]->Array.keepSome->intersperseOp->parenthesise}
    </span>
  }
}

module Nat = {
  module Base = AssocCommBase.Nat
  module Atom = Make(
    AssocCommBase.Nat,
    {
      let coerce = (AtomBase.AnyValue(tag, a)) =>
        switch tag {
        | Symbolic.Base.Tag => Int.fromString(a)->Option.map(Base.const)
        | AtomBase.VarBase.Tag =>
          switch a {
          | Var({idx}) => Some(Base.var(idx))
          | Schematic({schematic, allowed}) => Some(Base.schematic(schematic, allowed))
          }
        | AtomBase.String.Tag =>
          switch a {
          | [String(s)] => Int.fromString(s)->Option.map(Base.const)
          | _ => None
          }
        | _ => None
        }
    },
  )
  module AtomView = MakeView(AssocCommBase.Nat, Atom)
}
