open Signatures
open Util
module Context = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module Rule = Rule.Make(Term, Judgment)
  type rec t = {
    fixes: array<Term.meta>,
    facts: Dict.t<Rule.t>,
  }
}

module type PROOF_METHOD = {
  module Term: TERM
  module Judgment: JUDGMENT with module Term := Term
  module Rule: module type of Rule.Make(Term, Judgment)
  module Context: module type of Context(Term, Judgment)
  type t<'a>
  let keywords: array<string>
  let check: (t<'a>, Context.t, Judgment.t, ('a, Rule.t) => 'b) => result<t<'b>, string>
  let uncheck: (t<'a>, 'a => 'b) => t<'b>
  let parse: (
    string,
    ~keyword: string,
    ~scope: array<Term.meta>,
    ~gen: Term.gen,
    ~subparser: (string, ~scope: array<Term.meta>, ~gen: Term.gen) => result<('a, string), string>,
  ) => result<(t<'a>, string), string>
  let prettyPrint: (
    t<'a>,
    ~scope: array<Term.meta>,
    ~indentation: int=?,
    ~subprinter: ('a, ~scope: array<Term.meta>, ~indentation: int=?) => string,
  ) => string
}

module Derivation = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module Rule = Rule.Make(Term, Judgment)
  module Context = Context(Term, Judgment)
  type t<'a> = {
    ruleName: string,
    instantiation: array<Term.t>,
    subgoals: array<'a>,
  }
  let uncheck = (it: t<'a>, f) => {
    {
      ruleName: it.ruleName,
      instantiation: it.instantiation,
      subgoals: it.subgoals->Array.map(f),
    }
  }
  exception InternalParseError(string)
  let keywords = ["by"]
  let prettyPrint = (
    it: t<'a>,
    ~scope,
    ~indentation=0,
    ~subprinter: ('a, ~scope: array<Term.meta>, ~indentation: int=?) => string,
  ) => {
    let args = it.instantiation->Array.map(t => Term.prettyPrint(t, ~scope))
    "by ("
    ->String.concat(Array.join([it.ruleName, ...args], " "))
    ->String.concat(") {")
    ->String.concat(
      if Array.length(it.subgoals) > 0 {
        newline
      } else {
        ""
      },
    )
    ->String.concat(
      it.subgoals
      ->Array.map(s => subprinter(s, ~scope, ~indentation=indentation + 2))
      ->Array.join(newline),
    )
    ->String.concat("}")
  }
  let parse = (input, ~keyword as _, ~scope, ~gen, ~subparser) => {
    let cur = ref(String.trim(input))
    if cur.contents->String.get(0) == Some("(") {
      switch Rule.parseRuleName(cur.contents->String.sliceToEnd(~start=1)) {
      | Ok((ruleName, rest)) => {
          cur := rest
          let instantiation = []
          let it = ref(Error(""))
          while {
            it := Term.parse(cur.contents, ~scope, ~gen)
            it.contents->Result.isOk
          } {
            let (val, rest) = it.contents->Result.getExn
            Array.push(instantiation, val)
            cur := String.trim(rest)
          }
          if cur.contents->String.get(0) == Some(")") {
            cur := String.trim(cur.contents->String.sliceToEnd(~start=1))
            let subgoals = []
            if cur.contents->String.get(0) == Some("{") {
              cur := String.trim(cur.contents->String.sliceToEnd(~start=1))
              try {
                while cur.contents->String.get(0) != Some("}") {
                  switch subparser(cur.contents, ~scope, ~gen) {
                  | Ok((sg, rest)) => {
                      Array.push(subgoals, sg)
                      cur := String.trim(rest)
                    }
                  | Error(e) => raise(InternalParseError(e))
                  }
                }
                if cur.contents->String.get(0) == Some("}") {
                  cur := String.trim(cur.contents->String.sliceToEnd(~start=1))
                  Ok(({ruleName, instantiation, subgoals}, cur.contents))
                } else {
                  Error("} or subgoal proof expected")
                }
              } catch {
              | InternalParseError(e) => Error(e)
              }
            } else {
              Error("{ expected")
            }
          } else {
            Error(") or term expected")
          }
        }
      | Error(e) => Error(e)
      }
    } else {
      Error("Expected (")
    }
  }
  let check = (it: t<'a>, ctx: Context.t, j: Judgment.t, f: ('a, Rule.t) => 'b) => {
    switch ctx.facts->Dict.get(it.ruleName) {
    | None => Error("Cannot find rule '"->String.concat(it.ruleName)->String.concat("'"))
    | Some(rule) if Array.length(rule.vars) == Array.length(it.instantiation) => {
        let {premises, conclusion} = Rule.instantiate(rule, it.instantiation)
        if Judgment.equivalent(conclusion, j) {
          if Array.length(it.subgoals) == Array.length(premises) {
            Ok({
              ruleName: it.ruleName,
              instantiation: it.instantiation,
              subgoals: Belt.Array.zipBy(it.subgoals, premises, f),
            })
          } else {
            Error("Incorrect number of subgoals")
          }
        } else {
          let concString = Judgment.prettyPrint(conclusion, ~scope=ctx.fixes)
          let goalString = Judgment.prettyPrint(j, ~scope=ctx.fixes)
          Error(
            "Conclusion of rule '"
            ->String.concat(concString)
            ->String.concat("' doesn't match goal '")
            ->String.concat(goalString)
            ->String.concat("'"),
          )
        }
      }
    | _ => Error("Incorrect number of binders")
    }
  }
}
module Lemma = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module Rule = Rule.Make(Term, Judgment)
  module Context = Context(Term, Judgment)
  type t<'a> = {
    rule: Rule.t,
    proof: 'a,
    show: 'a,
  }
  let uncheck = (it: t<'a>, f) => {
    {
      rule: it.rule,
      proof: f(it.proof),
      show: f(it.show),
    }
  }
  let keywords = ["have"]
  let prettyPrint = (
    it: t<'a>,
    ~scope,
    ~indentation=0,
    ~subprinter: ('a, ~scope: array<Term.meta>, ~indentation: int=?) => string,
  ) => {
    "have "
    ->String.concat(Rule.prettyPrintInline(it.rule, ~scope))
    ->String.concat(newline)
    ->String.concat(subprinter(it.proof, ~scope, ~indentation))
    ->String.concat(newline)
    ->String.concat(subprinter(it.show, ~scope, ~indentation))
  }
  let parse = (input, ~keyword as _, ~scope, ~gen, ~subparser) => {
    //todo add toplevel
    switch Rule.parseInner(input, ~scope, ~gen) {
    | Ok((rule, rest)) =>
      switch subparser(rest, ~scope, ~gen) {
      | Ok((proof, rest')) =>
        switch String.trim(rest')->subparser(~scope, ~gen) {
        | Ok((show, rest'')) => Ok({rule, proof, show}, rest'')
        | Error(e) => Error(e)
        }
      | Error(e) => Error(e)
      }
    | Error(e) => Error(e)
    }
  }
  let check = (it: t<'a>, _ctx: Context.t, j: Judgment.t, f: ('a, Rule.t) => 'b) => {
    let first = f(it.proof, it.rule)
    let second = f(it.show, {vars: [], premises: [it.rule], conclusion: j})
    Ok({rule: it.rule, proof: first, show: second})
  }
}
module Combine = (
  Term: TERM,
  Judgment: JUDGMENT with module Term := Term,
  Method1: PROOF_METHOD with module Term := Term and module Judgment := Judgment,
  Method2: PROOF_METHOD with module Term := Term and module Judgment := Judgment,
) => {
  module Rule = Rule.Make(Term, Judgment)
  module Context = Context(Term, Judgment)
  type t<'a> = First(Method1.t<'a>) | Second(Method2.t<'a>)
  let uncheck = (it, f) =>
    switch it {
    | First(m) => First(Method1.uncheck(m, f))
    | Second(m) => Second(Method2.uncheck(m, f))
    }
  let keywords = Array.concat(Method1.keywords, Method2.keywords)

  let check = (it, ctx, j, f) =>
    switch it {
    | First(m) => m->Method1.check(ctx, j, f)->Result.map(x => First(x))
    | Second(m) => m->Method2.check(ctx, j, f)->Result.map(x => Second(x))
    }
  let prettyPrint = (it: t<'a>, ~scope, ~indentation=0, ~subprinter) =>
    switch it {
    | First(m) => m->Method1.prettyPrint(~scope, ~indentation, ~subprinter)
    | Second(m) => m->Method2.prettyPrint(~scope, ~indentation, ~subprinter)
    }
  let parse = (input, ~keyword, ~scope, ~gen, ~subparser) => {
    if Method1.keywords->Array.indexOf(keyword) > -1 {
      Method1.parse(input, ~keyword, ~scope, ~gen, ~subparser)->Result.map(((x, r)) => (
        First(x),
        r,
      ))
    } else {
      Method2.parse(input, ~keyword, ~scope, ~gen, ~subparser)->Result.map(((x, r)) => (
        Second(x),
        r,
      ))
    }
  }
}
