module Base = {
  type t = AtomBase.anyValue
  type AtomBase.atomTag<_> += Tag: AtomBase.atomTag<t>
  let wrap = t => t
  let wrapForReal = t => AtomBase.AnyValue(Tag, t)
}

type AtomBase.atomTag<_> += LengthTag: AtomBase.atomTag<StringA.t>

exception ShouldNotCallPrelude
exception NonPreludicValuePassed
module Atom = {
  module Base = Base
  type t = AtomBase.anyValue
  type subst = Map.t<int, t>
  // this is probably wrong: we want to unify with any nat
  let unify = (AtomBase.AnyValue(tagA, a), AtomBase.AnyValue(tagB, b), ~gen=?) => {
    switch (tagA, tagB) {
    | (LengthTag, LengthTag) =>
      StringA.Atom.unify(a, b, ~gen?)->Seq.map(subst =>
        subst->Util.mapMapValues(s => AtomBase.AnyValue(AtomBase.String.Tag, s))
      )
    | _ => Seq.empty
    }
  }
  let prettyPrint = (AtomBase.AnyValue(tag, a), ~scope: array<string>) => {
    switch tag {
    | LengthTag => `length(${StringA.Atom.prettyPrint(a, ~scope)})`
    | _ => throw(NonPreludicValuePassed)
    }
  }
  let parse = (str, ~scope: array<string>, ~gen=?) => {
    open Parser
    liftParse(StringA.Atom.parse, ~scope, ~gen?)
    ->between(token("length("), token(")"))
    ->map(s => AtomBase.AnyValue(LengthTag, s)->Base.wrapForReal)
    ->runParser(str)
  }

  let substitute = (AtomBase.AnyValue(tag, a), subst) => {
    switch tag {
    | LengthTag =>
      AtomBase.AnyValue(
        LengthTag,
        a->StringA.Atom.substitute(subst->Util.Map.filterMap((_, v) => StringA.Atom.coerce(v))),
      )->Base.wrapForReal
    | _ => throw(NonPreludicValuePassed)
    }
  }
  let substDeBruijn = (AtomBase.AnyValue(tag, a), subst, ~from=?) => {
    switch tag {
    | LengthTag => {
        let subst =
          a->StringA.Atom.substDeBruijn(
            subst->Array.map(v => v->Option.flatMap(StringA.Atom.coerce)),
            ~from?,
          )

        AtomBase.AnyValue(LengthTag, subst)->Base.wrapForReal
      }
    | _ => throw(NonPreludicValuePassed)
    }
  }

  let reduce = atom => {
    let AtomBase.AnyValue(tag, a) = atom
    switch tag {
    | LengthTag =>
      if (
        a->Array.every(p =>
          switch p {
          | AtomBase.String.String(_) => true
          | _ => false
          }
        )
      ) {
        AtomBase.AnyValue(AssocCommBase.Nat.Tag, AssocCommBase.Nat.const(Array.length(a)))
      } else {
        atom
      }
    | _ => throw(NonPreludicValuePassed)
    }
  }
  let concrete = _ => true
  let upshift = (AtomBase.AnyValue(tag, a), amount, ~from=?) => {
    switch tag {
    | LengthTag => AtomBase.AnyValue(LengthTag, a->StringA.Atom.upshift(amount, ~from?))->Base.wrap
    | _ => throw(NonPreludicValuePassed)
    }
  }
  let coerce = atom => Some(atom)
}

module AtomView: AtomDef.ATOM_VIEW with module Atom := Atom = {
  type props = {atom: Atom.t, scope: array<string>}
  let make = ({atom, scope}) => {
    let AtomBase.AnyValue(tag, a) = atom
    switch tag {
    | LengthTag =>
      <span>
        <span className="symbol"> {React.string("length(")} </span>
        {StringA.AtomView.make({atom: a, scope})}
        <span className="symbol"> {React.string(")")} </span>
      </span>
    | _ => throw(NonPreludicValuePassed)
    }
  }
}
