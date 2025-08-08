open Signatures
open Zora

let stringifyExn = (t: 'a) => JSON.stringifyAny(t, ~space=2)->Option.getExn

module MakeTerm = (Term: TERM) => {
  let termEquivalent = (t: Zora.t, t1: Term.t, t2: Term.t, ~msg=?) => {
    t->ok(
      Term.equivalent(t1, t2),
      ~msg=msg->Option.getOr(`${stringifyExn(t1)} equivalent to ${stringifyExn(t2)}`),
    )
  }
  let testParse = (t: Zora.t, input: string, t2: Term.t, ~scope=[], ~msg=?) => {
    let res = Term.parse(input, ~scope, ~gen=Term.makeGen())
    switch res {
    | Ok(res) => {
        t->equal(res->snd, "", ~msg=input ++ " input consumed")
        // NOTE: we're checking for equality here, not equivalency
        // error messages are better this way
        t->equal(res->fst, t2, ~msg?)
      }
    | Error(msg) => t->fail(~msg="parse failed: " ++ msg)
    }
  }
  let testParseFail = (t: Zora.t, input: string, ~scope=[], ~msg=?) => {
    let res = Term.parse(input, ~scope, ~gen=Term.makeGen())
    switch res {
    | Ok((p, remaining)) =>
      t->fail(
        ~msg=`parse intended to fail, but succeeded: ${Term.prettyPrint(
            p,
            ~scope,
          )}\nremaining: ${remaining}`,
      )
    | Error(_) => t->ok(true)
    }
  }

  let substEqual = (s1: Term.subst, s2: Term.subst) => {
    Map.size(s1) == Map.size(s1) &&
      Util.mapIntersection(s1, s2)
      ->Map.values
      ->Iterator.toArray
      ->Array.filter(((a, b)) => a == b)
      ->Array.length == Map.size(s2)
  }

  let substPrettyPrint = (subst: Term.subst) => {
    Util.mapMapValues(subst, t => Term.prettyPrint(t, ~scope=[]))
    ->Map.entries
    ->Iterator.toArray
    ->Array.map(Util.showTuple)
    ->Util.showArray
  }

  let substArrayPrettyPrint = (ss: array<Term.subst>) => {
    ss->Array.map(substPrettyPrint)->Util.showArray
  }

  let testUnify = (t: Zora.t, t1: Term.t, t2: Term.t, expect: array<Term.subst>, ~msg=?) => {
    let res = Term.unify(t1, t2)
    t->equal(
      Array.length(expect),
      Array.length(res),
      ~msg=`solutions: ${substArrayPrettyPrint(res)}\n\
      should have same length as expect: ${substArrayPrettyPrint(expect)}\n`,
    )
    let noMatches =
      res
      ->Array.filter(sub1 => Array.find(expect, sub2 => substEqual(sub1, sub2))->Option.isNone)
      ->Array.map(substPrettyPrint)
    let msg = msg->Option.getOr("each substitution should have a match in `expect`")
    t->equal(noMatches, [], ~msg)
  }
}
