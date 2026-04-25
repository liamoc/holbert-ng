type sourceloc = {
  line: int,
  col: int,
  idx: int,
}

type info = {
  msg?: string,
  expects: array<string>,
}

type err = {
  info: info,
  pos: sourceloc,
}

type state = {pos: sourceloc}

type t<'a> = (string, state) => result<('a, state), err>

let infoPretty = (info: info) => {
  let expectedMsg = switch info.expects {
  | [] => None
  | [expect] => Some(`expected ${expect}`)
  | _ => {
      let n = Array.length(info.expects)
      let first = info.expects->Array.slice(~start=0, ~end=n - 1)->Array.join(", ")
      Some(`expected ${first}, or ${info.expects[n - 1]->Option.getUnsafe}`)
    }
  }
  switch (info.msg, expectedMsg) {
  | (Some(infoMsg), Some(expectedMsg)) => `parse failed: ${infoMsg}\n  ${expectedMsg}`
  | (None, Some(msg)) | (Some(msg), None) => `parse failed: ${msg}`
  | (None, None) => `parse failed`
  }
}
let initialState = {pos: {line: 1, col: 1, idx: 0}}
let runParser = (p: t<'a>, str: string): result<('a, string), string> => {
  p(str, initialState)
  ->Result.map(((res, state)) => (res, str->String.sliceToEnd(~start=state.pos.idx)))
  ->Result.mapError(err => {
    `around ${str->String.slice(~start=err.pos.idx, ~end=err.pos.idx + 5)}:\n${infoPretty(
        err.info,
      )}`
  })
}

let map = (p: t<'a>, f: 'a => 'b): t<'b> =>
  (str, state) => p(str, state)->Result.map(((res, state)) => (f(res), state))
let mapError = (p: t<'a>, f: err => err) => (str, state) => p(str, state)->Result.mapError(f)
let label = (p: t<'a>, label: string) =>
  p->mapError(err => {...err, info: {...err.info, expects: [label]}})
let pure = (a): t<'a> => (_, state) => Ok((a, state))
let bind = (p1: t<'a>, p2: 'a => t<'b>): t<'b> =>
  (str, state) => p1(str, state)->Result.flatMap(((res, state)) => p2(res)(str, state))
let apply = (p: t<'a>, pf: t<'a => 'b>): t<'b> => p->bind(a => pf->map(f => f(a)))
let then = (p1: t<'a>, p2: t<'b>): t<'b> => p1->bind(_ => p2)
let thenIgnore = (p1: t<'a>, p2: t<'b>): t<'a> => p1->bind(res => p2->map(_ => res))

let fail = (info): t<'a> => (_, state) => Error({info: {msg: info, expects: []}, pos: state.pos})
let expected = (expects: array<string>, ~msg=?): t<'a> =>
  (_, state) => Error({info: {?msg, expects}, pos: state.pos})
let void = (p: t<'a>): t<unit> => p->map(_ => ())
// backtracks by default
let or = (p1: t<'a>, p2: t<'a>): t<'a> =>
  (str, state) =>
    switch p1(str, state) {
    | Ok(r) => Ok(r)
    | Error(e1) =>
      p2(str, state)->Result.mapError(e2 => {
        ...e2,
        info: {expects: Array.concat(e1.info.expects, e2.info.expects)},
      })
    }
let optional = (p: t<'a>): t<option<'a>> => p->map(a => Some(a))->or(pure(None))
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
let sepBy = (t, delim) => t->bind(p => many(delim->then(t))->map(ps => Array.concat([p], ps)))
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
      expected([`"${s}"`])
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
    | None => expected([`regex pattern ${re->RegExp.source}`])
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

let peek = (n): t<string> =>
  (str, state) => {
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

let dbg = (p: t<'a>, label): t<'a> => {
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
  ->mapError(err => {
    Console.log(("error", infoPretty(err.info)))
    err
  })
}

let lexeme = p => p->thenIgnore(regex(/^\s*/)->void)
let token = s => string(s)->lexeme

let decimal = regex1(/(\d+)/)->map(xStr => xStr->Int.fromString->Option.getExn)
let whitespace = regex(/\s*/)->void

let lift = (f: string => result<('a, string), string>): t<'a> =>
  getCurrentStr->bind(str =>
    switch f(str) {
    | Ok((res, remaining)) => {
        let length = String.length(str) - String.length(remaining)
        consume(length)->map(_ => res)
      }
    | Error(msg) => fail(msg)
    }
  )
let liftParse = (
  f: (string, ~scope: array<'m>, ~gen: 'g=?) => result<('a, string), string>,
  ~scope: array<'m>,
  ~gen: option<'g>=?,
): t<'a> => lift(s => f(s, ~scope, ~gen?))

module Util = {
  let varLit = string("\\")->then(decimal)->lexeme
  let ident = regex1(/([^\s\(\)]+)/)->lexeme
  let varIdx = (~scope: array<string>) =>
    varLit->or(
      ident->bind(id =>
        switch scope->Array.indexOfOpt(id) {
        | Some(idx) => pure(idx)
        | None => expected(["in-scope variable"])
        }
      ),
    )
  let schemaLit = (~scope: array<string>) =>
    string("?")
    ->then(decimal)
    ->bind(schematic =>
      many(varIdx(~scope))
      ->between(token("("), token(")"))
      ->map(allowed => {
        (schematic, allowed)
      })
    )
}
