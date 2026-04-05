module Base = AtomBase.Make({
  type t = string
})

module Atom = {
  module Base = Base
  type t = string
  type subst = Map.t<int, string>
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
  let concrete = _ => true
  let upshift = (t, _, ~from as _=?) => t
  let coerce = _ => None
}

module AtomView = {
  type props = {atom: string, scope: array<string>}
  let make = (props: props) => React.string(props.atom)
}
