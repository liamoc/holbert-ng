open AtomBase

module type ATOM = {
  module Base: BASE_ATOM
  type t = Base.t
  type subst = Map.t<int, t>
  let unify: (t, t, ~gen: ref<int>=?) => Seq.t<subst>
  let prettyPrint: (t, ~scope: array<string>) => string
  let parse: (string, ~scope: array<string>, ~gen: ref<int>=?) => result<(t, string), string>
  let substitute: (t, subst) => t
  let upshift: (t, int, ~from: int=?) => t
  let substDeBruijn: (t, array<option<t>>, ~from: int=?) => t
  let concrete: t => bool
  let coerce: anyValue => option<t>
}

exception AtomExpected

module AtomChoiceBase = Make({
  type t = anyValue
})

module type ATOM_CHOICE = {
  module LeftBase: BASE_ATOM
  include ATOM with module Base = AtomChoiceBase
  let onLeft: (t, LeftBase.t => 'a) => option<'a>
}

module EmptyAtomChoice: ATOM_CHOICE = {
  module LeftBase = AtomBase.Make({
    // empty
    type t = {.}
  })
  module Base = AtomChoiceBase
  type t = Base.t
  type subst = Map.t<int, t>
  let parse = (_, ~scope as _, ~gen as _=?) => Error("expected atom")
  let unify = (_, _, ~gen as _=?) => Seq.empty
  // this should probably throw too, but will be more
  // informative to have it appear wherever it's called from
  let prettyPrint = (_, ~scope as _) => "NIL (THIS IS AN ERROR!)"
  let onLeft = (_, _) => throw(AtomExpected)
  let coerce = _ => throw(AtomExpected)
  let substitute = (_, _) => throw(AtomExpected)
  let upshift = (_, _, ~from as _=?) => throw(AtomExpected)
  let substDeBruijn = (_, _, ~from as _=?) => throw(AtomExpected)
  let concrete = _ => throw(AtomExpected)
}

module MakeAtomChoice = (Left: ATOM, Right: ATOM_CHOICE): (
  ATOM_CHOICE with module LeftBase = Left.Base
) => {
  module LeftBase = Left.Base
  module Right = Right
  module Base = AtomChoiceBase
  type t = Base.t
  type subst = Map.t<int, t>
  type gen = ref<int>
  let getOrElse = Util.Option.getOrElse
  let coerce = v => Some(v)
  let onLeft = (AnyValue(tag, val), f: Left.t => 'a): option<'a> =>
    switch tag {
    | Left.Base.Tag => Some(f(val))
    | _ => None
    }
  let parse = (s, ~scope, ~gen: option<gen>=?) => {
    Left.parse(s, ~scope, ~gen?)
    ->Result.map(((r, rest)) => (LeftBase.wrap(r), rest))
    ->Util.Result.or(() => Right.parse(s, ~scope, ~gen?))
  }
  let prettyPrint = (atom, ~scope) =>
    atom
    ->onLeft(val => Left.prettyPrint(val, ~scope))
    ->getOrElse(() => Right.prettyPrint(atom, ~scope))

  let unify = (a1, a2, ~gen=?) => {
    let (AnyValue(tag1, val1), AnyValue(tag2, val2)) = (a1, a2)
    switch (tag1, tag2) {
    | (Left.Base.Tag, Left.Base.Tag) =>
      Left.unify(val1, val2)->Seq.map(subst => subst->Util.mapMapValues(LeftBase.wrap))
    | (_, _) => Right.unify(a1, a2, ~gen?)
    }
  }
  let coerceToLeft = (atom): option<Left.t> =>
    atom->onLeft(val => Some(val))->getOrElse(() => Left.coerce(atom))
  let substitute = (atom, subst: subst) =>
    atom
    ->onLeft(val => {
      let leftSubs = subst->Util.Map.filterMap((_, v) => coerceToLeft(v))
      Left.substitute(val, leftSubs)->LeftBase.wrap
    })
    ->getOrElse(() => Right.substitute(atom, subst))

  let upshift = (atom, amount: int, ~from=?) =>
    atom
    ->onLeft(val => Left.upshift(val, amount, ~from?)->LeftBase.wrap)
    ->getOrElse(() => Right.upshift(atom, amount, ~from?))
  let substDeBruijn = (atom, substs: array<option<t>>, ~from=?) =>
    atom
    ->onLeft(val =>
      Left.substDeBruijn(
        val,
        substs->Array.map(o => o->Option.flatMap(coerceToLeft)),
        ~from?,
      )->LeftBase.wrap
    )
    ->getOrElse(() => Right.substDeBruijn(atom, substs, ~from?))
  let concrete = atom => atom->onLeft(Left.concrete)->getOrElse(() => Right.concrete(atom))
}

module type ATOM_VIEW = {
  module Atom: ATOM
  type props = {atom: Atom.t, scope: array<string>}
  let make: props => React.element
}

module EmptyAtomChoiceView: ATOM_VIEW with module Atom := EmptyAtomChoice = {
  type props = {atom: EmptyAtomChoice.t, scope: array<string>}
  let make = _ => throw(AtomExpected)
}

module MakeAtomChoiceView = (
  Left: ATOM,
  LeftView: ATOM_VIEW with module Atom := Left,
  Right: ATOM_CHOICE,
  RightView: ATOM_VIEW with module Atom := Right,
  Combined: module type of MakeAtomChoice(Left, Right),
): (ATOM_VIEW with module Atom := Combined) => {
  type props = {atom: Combined.t, scope: array<string>}
  let make = ({atom, scope}: props) =>
    atom
    ->Combined.onLeft(left => <LeftView atom={left} scope />)
    ->Util.Option.getOrElse(() => <RightView atom scope />)
}

module MakeAtomChoiceAndView = (
  Left: ATOM,
  LeftView: ATOM_VIEW with module Atom := Left,
  Right: ATOM_CHOICE,
  RightView: ATOM_VIEW with module Atom := Right,
) => {
  module Atom = MakeAtomChoice(Left, Right)
  module AtomView = MakeAtomChoiceView(Left, LeftView, Right, RightView, Atom)
}
