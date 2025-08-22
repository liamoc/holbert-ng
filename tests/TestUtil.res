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
  let testParsePrettyPrint = (t: Zora.t, input, expected, ~scope=[]) => {
    let res = Term.parse(input, ~scope=[], ~gen=Term.makeGen())

    switch res {
    | Ok(res) => {
        let result = Term.prettyPrint(res->fst, ~scope)
        t->equal(result, expected, ~msg="prettyPrint output matches expected")
      }
    | Error(msg) => t->fail(~msg="parse failed: " ++ msg)
    }
  }

  let substArrayPrettyPrint = (ss: array<Term.subst>) => {
    ss->Array.map(t => Term.prettyPrintSubst(t, ~scope=[]))->Util.showArray
  }

  let testUnify = (t: Zora.t, t1: Term.t, t2: Term.t, expect: array<Term.subst>, ~msg=?) => {
    let expect = Seq.fromArray(expect)
    let res = Term.unify(t1, t2)->Seq.take(10)
    Console.log(res->Seq.map(t => Term.prettyPrintSubst(t, ~scope=[]))->Seq.join(","))
    let noMatches =
      expect
      ->Seq.filter(sub1 => Seq.find(res, sub2 => Term.substEqual(sub1, sub2))->Option.isNone)
      ->Seq.map(t => Term.prettyPrintSubst(t, ~scope=[]))
      ->Seq.toArray
    let msg = msg->Option.getOr("each substitution in `expect` should have a match in solutions")
    t->equal(noMatches, [], ~msg)
  }

  let testUnifyFail = (t: Zora.t, at: string, bt: string, ~msg=?) => {
    let gen = Term.makeGen()
    let (a, _) = Term.parse(at, ~scope=[], ~gen)->Result.getExn
    let (b, _) = Term.parse(bt, ~scope=[], ~gen)->Result.getExn
    let res = Term.unify(a, b)
    if res->Seq.length != 0 {
      t->fail(~msg="unification succeeded: " ++ stringifyExn(a) ++ " and " ++ stringifyExn(b))
    } else {
      t->ok(true, ~msg=msg->Option.getOr("unification failed"))
    }
  }
}
