module ConstSymbol: SExpFunc.SYMBOL with type t = string = {
  type t = string
  type subst = Map.t<int, string>
  let unify = (a, b) =>
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
  let lowerVar = _ => ""
  let lowerSchematic = (_, _) => ""
  let ghost = ""
  let substDeBruijn = (name, _, ~from as _) => name
  let constSymbol = name => Some(name)
}

include SExpFunc.Make(ConstSymbol)
let pSymbol = s => s->ConstSymbol.parse(~scope=[])->Result.getExn->Pair.first
let symbol = s => s->pSymbol->Symbol
let getSymbol = s => s->ConstSymbol.prettyPrint(~scope=[])
