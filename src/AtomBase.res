// type level stuff to enable well-typed coercions
type atomTag<_> = ..
type rec anyValue = AnyValue(atomTag<'a>, 'a): anyValue

// to allow circular coercions, we declare base types
// separately from relevant implementation
module type BASE_ATOM = {
  type t
  type atomTag<_> += Tag: atomTag<t>
  let wrap: t => anyValue
}

module Make = (
  T: {
    type t
  },
): (BASE_ATOM with type t = T.t) => {
  type t = T.t
  type atomTag<_> += Tag: atomTag<t>
  let wrap = t => AnyValue(Tag, t)
}

module String = {
  type piece =
    | String(string)
    | Var({idx: int})
    | Schematic({schematic: int, allowed: array<int>})
  include Make({type t = array<piece>})
}

module VarBase = {
  type varBase = Var({idx: int}) | Schematic({schematic: int, allowed: array<int>})
  include Make({type t = varBase})
}
