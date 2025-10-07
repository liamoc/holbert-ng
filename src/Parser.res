type sourceloc = {
  line: int,
  col: int,
  idx: int,
}

type err<'info> = {
  message: 'info,
  pos: sourceloc,
}

type state = {pos: sourceloc}

type t<'a> = (string, state) => result<('a, state), err<string>>

let initialState = {pos: {line: 1, col: 1, idx: 0}}
let runParser = (p: t<'a>, str: string): result<('a, string), err<string>> => {
  p(str, initialState)->Result.map(((res, state)) => (
    res,
    str->String.sliceToEnd(~start=state.pos.idx),
  ))
}

let map = (p: t<'a>, f: 'a => 'b): t<'b> => (str, state) =>
  p(str, state)->Result.map(((res, state)) => (f(res), state))
let pure = (a): t<'a> => (_, state) => Ok((a, state))
let bind = (p1: t<'a>, p2: 'a => t<'b>): t<'b> => (str, state) =>
  p1(str, state)->Result.flatMap(((res, state)) => p2(res)(str, state))
let apply = (p: t<'a>, pf: t<'a => 'b>): t<'b> => p->bind(a => pf->map(f => f(a)))
let then = (p1: t<'a>, p2: t<'b>): t<'b> => p1->bind(_ => p2)
let thenIgnore = (p1: t<'a>, p2: t<'b>): t<'a> => p1->bind(res => p2->map(_ => res))

let fail = (info): t<'a> => (_, state) => Error({message: info, pos: state.pos})
let void = (p: t<'a>): t<unit> => p->map(_ => ())
// backtracks by default
let optional = (p: t<'a>): t<option<'a>> => (str, state) => {
  switch p(str, state) {
  | Ok((res, state)) => Ok((Some(res), state))
  | Error(_) => Ok((None, state))
  }
}
let or = (p1: t<'a>, p2: t<'a>): t<'a> => (str, state) =>
  switch p1(str, state) {
  | Ok(r) => Ok(r)
  | Error(_) => p2(str, state)
  }
let choice = (ps: array<t<'a>>): t<'a> => {
  ps->Array.reduce(fail("no matches"), or)
}

let getState: t<state> = (_, state) => Ok((state, state))
let setState = (newState: state): t<unit> => (_, _) => Ok(((), newState))
let modifyState = (f: state => state) => getState->bind(state => setState(f(state)))
let readStr: t<string> = (str, state) => Ok((str, state))
let getCurrentStr: t<string> = (str, state) => Ok((
  str->String.sliceToEnd(~start=state.pos.idx),
  state,
))

let eof: t<bool> = getState->bind(state => readStr->map(str => state.pos.idx >= str->String.length))

// fixpoints using references as a layer of indirection are necessary because
// the compiler complains very ambiguously about not knowing the size of
// (directly) recursive combinators
let fix = (f: t<'a> => t<'a>): t<'a> => {
  let pRef = ref(fail("umm"))
  pRef := f((str, state) => pRef.contents(str, state))
  pRef.contents
}

let many = p =>
  fix(f =>
    optional(p)->bind(o =>
      switch o {
      | Some(res) => f->map(l => l->List.add(res))
      | None => pure(list{})
      }
    )
  )->map(List.toArray)
let between = (inner: t<'a>, o1: t<string>, o2: t<string>): t<'a> => o1->then(inner)->thenIgnore(o2)
let consume = (l: int): t<string> =>
  getCurrentStr->bind(str => {
    if str->String.length < l {
      fail("tried to consume too much")
    } else {
      let consumed = str->String.slice(~start=0, ~end=l)
      let vOffset = consumed->String.split("\n")->Array.length - 1
      let hOffset = consumed->String.length - consumed->String.lastIndexOfOpt("\n")->Option.getOr(0)
      modifyState(state => {
        pos: {
          col: vOffset > 0 ? hOffset : state.pos.col + hOffset,
          line: state.pos.line + vOffset,
          idx: state.pos.idx + l,
        },
      })->then(pure(consumed))
    }
  })

let execRe = (re, str) => {
  re
  ->RegExp.exec(str)
  ->Option.map(result => {
    open RegExp.Result
    (matches(result), fullMatch(result)->String.length)
  })
}

// this will be released in Core in 12.0.0.4, remove then
@get external regexpFlags: RegExp.t => string = "flags"

let string = s =>
  getCurrentStr->bind(str =>
    if str->String.startsWith(s) {
      consume(String.length(s))->then(pure(s))
    } else {
      fail(`${str->String.slice(~start=0, ~end=10)} doesn't start with string ${s}`)
    }
  )

let regex = (re: RegExp.t): t<array<string>> => {
  let wrapped = {
    let source = re->RegExp.source
    if source->String.startsWith("^") {
      re
    } else {
      RegExp.fromStringWithFlags(`^${source}`, ~flags=re->regexpFlags)
    }
  }
  getCurrentStr->bind(str =>
    switch execRe(wrapped, str) {
    | Some((matches, l)) => consume(l)->then(pure(matches))
    | None => fail("regex failed")
    }
  )
}

let regex1 = (re: RegExp.t): t<string> =>
  regex(re)->bind(matches =>
    switch matches {
    | [x] => pure(x)
    | _ => fail("more than one match")
    }
  )

let peek = (n): t<string> => (str, state) => {
  let res = str->String.slice(~start=state.pos.idx, ~end=state.pos.idx + n)
  Ok((res, state))
}

type length = int
let takeWhileMany = (f: string => option<length>) =>
  fix(p =>
    getCurrentStr->bind(str =>
      switch f(str) {
      | Some(length) => consume(length)->bind(s => p->map(s' => String.concat(s, s')))
      | None => pure("")
      }
    )
  )

let takeWhile = (f: string => bool) =>
  takeWhileMany(s =>
    if f(s) {
      Some(1)
    } else {
      None
    }
  )

let dbg = (p: t<'a>, ~label): t<'a> => {
  let dbgInfo = title =>
    getCurrentStr->bind(str =>
      getState->map(state => {
        let currSurr = `${str->String.slice(~start=0, ~end=10)}...`
        let stateStr = `${state.pos.line->Int.toString}:${state.pos.col->Int.toString}`
        Console.log(`${title} ${label} at:\n${currSurr}\nstate: ${stateStr}`)
      })
    )
  dbgInfo("enter")
  ->then(p)
  ->bind(res => {
    Console.log(("successfully parsed", res))
    dbgInfo("exit")->map(_ => res)
  })
}

let lexeme = p => p->thenIgnore(regex(%re(`/^\s*/`))->void)
let token = s => string(s)->lexeme

let decimal = regex1(%re(`/(\d+)/`))->map(xStr => xStr->Int.fromString->Option.getExn)
let whitespace = regex(%re(`/\s*/`))->void
