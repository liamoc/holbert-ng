open Signatures
// the tree sitter plugin hates backslashes in string literals unless they're on the top
// level..
let ruleNamePattern = "^[^|()\\s\\-—][^()\\s]*"
let vinculumRES = "^\s*\\n\\s*[-—][-—][\\-—]+[ \t]*([^()|\\s\\-—][^()\\s]*)?"
module Make = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  type rec t = {vars: array<Term.meta>, premises: array<t>, conclusion: Judgment.t}
  let rec substitute = (rule: t, subst: Judgment.subst) => {
    let subst' =
      subst->Judgment.mapSubst(v => v->Judgment.upshiftSubstCodom(Array.length(rule.vars)))
    {
      vars: rule.vars,
      premises: rule.premises->Array.map(premise => premise->substitute(subst')),
      conclusion: rule.conclusion->Judgment.substitute(subst')->Judgment.reduce,
    }
  }
  let rec substDeBruijn = (rule: t, substs: array<Judgment.substCodom>, ~from: int=0) => {
    let len = Array.length(rule.vars)
    let substs' = substs->Array.map(v => v->Judgment.upshiftSubstCodom(len, ~from))
    {
      vars: rule.vars,
      premises: rule.premises->Array.map(premise =>
        premise->substDeBruijn(substs', ~from=from + len)
      ),
      conclusion: rule.conclusion
      ->Judgment.substDeBruijn(substs', ~from=from + len)
      ->Judgment.reduce,
    }
  }
  let rec upshift = (rule: t, amount: int, ~from: int=0) => {
    let len = Array.length(rule.vars)
    {
      vars: rule.vars,
      premises: rule.premises->Array.map(r => r->upshift(amount, ~from=from + len)),
      conclusion: rule.conclusion->Judgment.upshift(amount, ~from=from + len),
    }
  }
  type bare = {premises: array<t>, conclusion: Judgment.t}
  let substituteBare = (rule: bare, subst: Judgment.subst) => {
    {
      premises: rule.premises->Array.map(premise => premise->substitute(subst)),
      conclusion: rule.conclusion->Judgment.substitute(subst)->Judgment.reduce,
    }
  }
  let instantiate = (rule: t, terms: array<Judgment.substCodom>) => {
    assert(Array.length(terms) == Array.length(rule.vars))
    let terms' = [...terms]
    Array.reverse(terms')
    {
      premises: rule.premises->Array.map(r => r->substDeBruijn(terms')),
      conclusion: rule.conclusion->Judgment.substDeBruijn(terms')->Judgment.reduce,
    }
  }
  let genSchemaInsts = (rule: t, gen: Term.gen, ~scope: array<Judgment.meta>) => {
    rule.vars->Array.map(m => Judgment.placeSubstCodom(gen->Term.fresh(~replacing=m), ~scope))
  }
  let parseRuleName = str => {
    let re = RegExp.fromStringWithFlags(ruleNamePattern, ~flags="y")
    switch re->RegExp.exec(String.trim(str)) {
    | None => Error("expected rule name")
    | Some(res) =>
      switch res[0] {
      | Some(Some(n)) if String.trim(n) != "" =>
        Ok(n, String.sliceToEnd(String.trim(str), ~start=RegExp.lastIndex(re)))
      | _ => Error("expected rule name")
      }
    }
  }
  let parseVinculum = str => {
    let re = RegExp.fromStringWithFlags(vinculumRES, ~flags="y")
    switch re->RegExp.exec(str) {
    | None => Error("expected vinculum")
    | Some(res) =>
      switch res[1] {
      | Some(Some(n)) if String.trim(n) != "" =>
        Ok(n, String.sliceToEnd(str, ~start=RegExp.lastIndex(re)))
      | _ => Ok("", String.sliceToEnd(str, ~start=RegExp.lastIndex(re)))
      }
    }
  }
  exception InternalParseError(string)
  let rec parseInner = (string, ~scope=[]: array<Term.meta>, ~gen=?) => {
    if string->String.trim->String.get(0) == Some("[") {
      let cur = ref(String.make(string->String.trim->String.sliceToEnd(~start=1)))
      let it = ref(Error(""))
      let vars = []
      while {
        it := Term.parseMeta(cur.contents)
        it.contents->Result.isOk
      } {
        let (str, rest) = Result.getExn(it.contents)
        Array.unshift(vars, str)
        cur := rest
      }
      let premises = []
      switch {
        while (
          cur.contents->String.trim->String.slice(~start=0, ~end=2) != "|-" &&
            cur.contents->String.trim->String.get(0) != Some("]")
        ) {
          switch parseInner(cur.contents, ~scope=vars->Array.concat(scope), ~gen?) {
          | Ok(p, rest) => {
              cur := rest
              premises->Array.push(p)
            }
          | Error(_) => raise(InternalParseError("expected turnstile or premise"))
          }
        }
        if cur.contents->String.trim->String.get(0) == Some("]") {
          let rest = cur.contents->String.trim->String.sliceToEnd(~start=1)
          cur := rest
          switch premises {
          | [{vars: [], premises: [], conclusion: e}] =>
            Ok(({vars, premises: [], conclusion: e}, rest))
          | _ => Error("Conclusion appears to be multiple terms")
          }
        } else {
          cur := cur.contents->String.trim->String.sliceToEnd(~start=2)
          switch Judgment.parse(cur.contents, ~scope=vars->Array.concat(scope), ~gen?) {
          | Ok(conclusion, rest) =>
            if rest->String.trim->String.get(0) == Some("]") {
              cur := rest->String.trim->String.sliceToEnd(~start=1)
              Ok(({vars, premises, conclusion}, cur.contents))
            } else {
              Error("Expected closing bracket")
            }
          | Error(e) => Error(e)
          }
        }
      } {
      | exception InternalParseError(e) => Error(e)
      | v => v
      }
    } else {
      switch Judgment.parse(string, ~scope, ~gen?) {
      | Ok(conclusion, rest) => Ok(({vars: [], premises: [], conclusion}, rest))
      | Error(e) => Error(e)
      }
    }
  }
  let parseTopLevel = (string, ~gen=?, ~scope=[]) => {
    let cur = ref(String.make(string))
    let it = ref(Error(""))
    let vars = []
    switch {
      while {
        it := Term.parseMeta(cur.contents)
        it.contents->Result.isOk
      } {
        let (str, rest) = Result.getExn(it.contents)
        Array.unshift(vars, str)
        cur := rest
      }
      let it = ref(Error(""))
      let premises = []
      while {
        it := parseVinculum(cur.contents)
        it.contents->Result.isError
      } {
        switch parseInner(cur.contents, ~scope=vars->Array.concat(scope), ~gen?) {
        | Ok(p, rest) => {
            cur := rest
            premises->Array.push(p)
          }
        | Error(e) => raise(InternalParseError(e))
        }
      }
      let (ruleName, rest) = it.contents->Result.getExn
      cur := rest
      switch Judgment.parse(cur.contents, ~scope=vars->Array.concat(scope), ~gen?) {
      | Ok(conclusion, rest) => Ok((({vars, premises, conclusion}, ruleName), rest))
      | Error(e) => Error(e)
      }
    } {
    | exception InternalParseError(e) => Error(e)
    | v => v
    }
  }

  let rec prettyPrintInline = (rule: t, ~scope=[]: array<Term.meta>) => {
    switch rule {
    | {vars: [], premises: [], conclusion: c} => Judgment.prettyPrint(c, ~scope)
    | _ => {
        let vars' = Array.copy(rule.vars)
        Array.reverse(vars')
        "["
        ->String.concat(
          vars'
          ->Array.map(Term.prettyPrintMeta)
          ->Array.join("")
          ->String.concat(" ")
          ->String.concat(
            if Array.length(rule.premises) == 0 {
              Judgment.prettyPrint(rule.conclusion, ~scope=[...rule.vars, ...scope])
            } else {
              rule.premises
              ->Array.map(r => prettyPrintInline(r, ~scope=[...rule.vars, ...scope]))
              ->Array.join(" ")
              ->String.concat(" |- ")
              ->String.concat(
                Judgment.prettyPrint(rule.conclusion, ~scope=[...rule.vars, ...scope]),
              )
            },
          ),
        )
        ->String.concat("]")
      }
    }
  }
  let prettyPrintTopLevel = (rule: t, ~name="", ~scope=[]: array<Term.meta>) => {
    let vinculise = (strs: array<string>) => {
      let l = strs->Array.map(String.length)->Array.concat([4])->Math.Int.maxMany
      strs->Array.concat(["-"->String.repeat(l)->String.concat(" ")->String.concat(name)])
    }
    let myReverse = arr => {
      Array.reverse(arr)
      arr
    }
    rule.vars
    ->Array.map(Term.prettyPrintMeta)
    ->myReverse
    ->Array.join("")
    ->String.concat(Util.newline)
    ->String.concat(
      rule.premises
      ->Array.map(r => prettyPrintInline(r, ~scope=[...rule.vars, ...scope]))
      ->vinculise
      ->Array.concat([Judgment.prettyPrint(rule.conclusion, ~scope=[...rule.vars, ...scope])])
      ->Array.map(s => String.concat("  ", s))
      ->Array.join(Util.newline),
    )
  }
}
