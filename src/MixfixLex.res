let wsRE = %re("/^\s*/")
let identRE = %re("/^[^\s.()\[\]][^\s.()\[\]]*/")

let charAt = (s: string, i: int): string => String.charAt(s, i)
let sliceToEnd = (s: string, ~start: int): string => String.sliceToEnd(s, ~start)

let skipWs = (s: string): string =>
  switch wsRE->RegExp.exec(s) {
  | Some(res) =>
    switch res[0] {
    | Some(Some(m)) => sliceToEnd(s, ~start=String.length(m))
    | _ => s
    }
  | None => s
  }

let takeIdent = (s: string): option<(string, string)> => {
  let s = skipWs(s)
  switch identRE->RegExp.exec(s) {
  | None => None
  | Some(res) =>
    switch res[0] {
    | Some(Some(tok)) => Some((tok, sliceToEnd(s, ~start=String.length(tok))))
    | _ => None
    }
  }
}

let lastIndexOf = (arr: array<string>, name: string): option<int> =>
  arr->Array.reduceWithIndex(None, (acc, m, i) => m == name ? Some(i) : acc)