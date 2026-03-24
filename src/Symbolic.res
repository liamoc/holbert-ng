type t = string
module Atom = {
  open AtomDef
  type t = string
  type subst = Map.t<int, string>
  type atomTag<_> += Tag: atomTag<t>
  let tagEq = (type a, tag: atomTag<a>): option<eq<t, a>> =>
    switch tag {
    | Tag => Some(Refl)
    | _ => None
    }
  let unify = (a, b, ~gen as _=?) =>
    if a == b {
      Seq.once(Map.make())
    } else {
      Seq.empty
    }
  let prettyPrint = (name, ~scope as _: array<string>) => name
  let symbolRegex = /^([^\s()\[\]]+)/
  let parse = (string, ~scope as _: array<string>, ~gen as _=?) =>
    switch Util.execRe(symbolRegex, string) {
    | Some(([res], l)) => Ok((res, string->String.substringToEnd(~start=l)))
    | _ => Error("constant symbol parse error")
    }
  let substitute = (name, _) => name
  let substDeBruijn = (name, _, ~from as _=?) => name
  let concrete = _ => false
  let upshift = (t, _, ~from as _=?) => t
}

module AtomView = {
  type props = {name: string, scope: array<string>}
  let make = (props: props) => React.string(props.name)
}
