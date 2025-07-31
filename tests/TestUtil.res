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
  let testParse = (t: Zora.t, input: string, t2: Term.t, ~msg=?) => {
    let res = Term.parse(input, ~scope=[], ~gen=Term.makeGen())
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
  let testUnify = (t: Zora.t, at: string, bt: string, ~subst=?, ~msg=?) => {
    let gen = Term.makeGen()
    let (a, _) = Term.parse(at, ~scope=[], ~gen)->Result.getExn
    let (b, _) = Term.parse(bt, ~scope=[], ~gen)->Result.getExn
    let res = Term.unify(a, b, ~gen)
    if res->Array.length == 0 {
      t->fail(~msg="unification failed: " ++ stringifyExn(a) ++ " and " ++ stringifyExn(b))
    } else {
      switch subst {
      | None => t->ok(true, ~msg=msg->Option.getOr("unification succeeded"))
      | Some(subst) => {
          t->equal(res->Array.length, 1)
          t->equal(
            res[0]->Option.getExn,
            subst,
            ~msg=msg->Option.getOr("unification succeeded with substitution"),
          )
        }
      }
    }
  }
  let testNotUnify = (t: Zora.t, at: string, bt: string, ~msg=?) => {
    let gen = Term.makeGen()
    let (a, _) = Term.parse(at, ~scope=[], ~gen)->Result.getExn
    let (b, _) = Term.parse(bt, ~scope=[], ~gen)->Result.getExn
    let res = Term.unify(a, b)
    if res->Array.length != 0 {
      t->fail(~msg="unification succeeded: " ++ stringifyExn(a) ++ " and " ++ stringifyExn(b))
    } else {
      t->ok(true, ~msg=msg->Option.getOr("unification failed"))
    }
  }
}
