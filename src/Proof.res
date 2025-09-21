open Method
open Signatures

module Make = (
  Term: TERM,
  Judgment: JUDGMENT with module Term := Term,
  Method: PROOF_METHOD with module Term := Term and module Judgment := Judgment,
) => {
  module Rule = Rule.Make(Term, Judgment)
  module Context = Context(Term, Judgment)
  type rec t = {
    fixes: array<Term.meta>,
    assumptions: array<string>,
    method: option<Method.t<t>>,
  }
  type rec checked =
    | Checked({
        fixes: array<Term.meta>,
        assumptions: array<string>,
        method: option<Method.t<checked>>,
        rule: Rule.t,
      })
    | ProofError({raw: t, rule: Rule.t, msg: string})
  let parseKeyword = input => {
    Method.keywords
    ->Array.concat(["?"])
    ->Array.find(kw => String.trim(input)->String.startsWith(kw))
  }
  let rec prettyPrint = (prf: t, ~scope, ~indentation=0) => {
    let mtd = switch prf.method {
    | None => "?"
    | Some(m) =>
      Method.prettyPrint(
        m,
        ~scope=prf.fixes->Array.concat(scope),
        ~indentation=indentation + 2,
        ~subprinter=prettyPrint,
      )
    }
    String.padStart("", indentation, " ")
    ->String.concat(prf.fixes->Array.map(Term.prettyPrintMeta)->Array.join(""))
    ->String.concat(
      prf.assumptions
      ->Array.map(s => String.concat(" ", s))
      ->Array.join(""),
    )
    ->String.concat(
      if Array.length(prf.assumptions) == 0 {
        "|- "
      } else {
        " |- "
      },
    )
    ->String.concat(mtd)
  }
  let rec parse = (input, ~scope, ~gen) => {
    let it = ref(Error(""))
    let cur = ref(String.trim(input))
    let fixes = []
    while {
      it := Term.parseMeta(cur.contents)
      it.contents->Result.isOk
    } {
      let (n, r) = Result.getExn(it.contents)
      cur := String.trim(r)
      fixes->Array.unshift(n)
    }
    let it = ref(Error(""))
    let assumptions = []
    while {
      it := Rule.parseRuleName(cur.contents)
      it.contents->Result.isOk
    } {
      let (a, r) = Result.getExn(it.contents)
      cur := String.trim(r)
      assumptions->Array.push(a)
    }
    if cur.contents->String.slice(~start=0, ~end=2) != "|-" {
      Console.log((fixes, assumptions))
      Error("expected turnstile or rule name"->String.concat(cur.contents))
    } else {
      cur := cur.contents->String.trim->String.sliceToEnd(~start=2)->String.trim
      let scope' = Array.concat(fixes, scope)
      switch parseKeyword(cur.contents) {
      | Some("?") =>
        Ok(({fixes, assumptions, method: None}, cur.contents->String.sliceToEnd(~start=1)))
      | Some(keyword) => {
          cur := cur.contents->String.sliceToEnd(~start=String.length(keyword))
          switch Method.parse(cur.contents, ~keyword, ~scope=scope', ~gen, ~subparser=parse) {
          | Ok((method, r)) => Ok(({fixes, assumptions, method: Some(method)}, r))
          | Error(e) => Error(e)
          }
        }
      | None => Error("Expected keyword")
      }
    }
  }
  let enter = (ctx: Context.t, prf: t, rule: Rule.t) => {
    let (nFixes, nVars) = (Array.length(prf.fixes), Array.length(rule.vars))
    if Array.length(prf.fixes) == Array.length(rule.vars) {
      let (nAssumptions, nPremises) = (Array.length(prf.assumptions), Array.length(rule.premises))
      if nAssumptions == nPremises {
        let newFacts = Dict.fromArray(Belt.Array.zip(prf.assumptions, rule.premises))
        Ok({
          Context.fixes: rule.vars->Array.concat(ctx.fixes),
          facts: Dict.copy(ctx.facts)->Dict.assign(newFacts),
        })
      } else {
        Error(
          `Proof introduces a different number (${Int.toString(
              nAssumptions,
            )}) of assumptions than the rule (${Int.toString(nPremises)})`,
        )
      }
    } else {
      Error(
        `Proof introduces a different number (${Int.toString(
            nFixes,
          )}) of variables than the rule (${Int.toString(nVars)})`,
      )
    }
  } //result<Context, string>

  let rec uncheck = (prf: checked) =>
    switch prf {
    | ProofError({raw, rule: _, msg: _}) => raw
    | Checked({fixes, assumptions, method, rule: _}) => {
        fixes,
        assumptions,
        method: method->Option.map(xs => xs->Method.uncheck(uncheck)),
      }
    }
  let rec check = (ctx: Context.t, prf: t, rule: Rule.t) => {
    let ruleStr = Rule.prettyPrintInline(rule, ~scope=[])
    Console.log(("CHECK", ctx, prf, ruleStr))
    switch enter(ctx, prf, rule) {
    | Ok(ctx') =>
      switch prf.method {
      | Some(m) =>
        switch m->Method.check(ctx', rule.conclusion, (s, r) => check(ctx', s, r)) {
        | Ok(m') =>
          Checked({
            rule,
            fixes: prf.fixes,
            assumptions: prf.assumptions,
            method: Some(m'),
          })
        | Error(e) => ProofError({raw: prf, rule, msg: e})
        }
      | None =>
        Checked({
          rule,
          fixes: prf.fixes,
          assumptions: prf.assumptions,
          method: None,
        })
      }
    | Error(e) => ProofError({raw: prf, rule, msg: e})
    }
  } // result<checked,string>
}

/*
  
    
  
 */
