open Zora
module P = Parser

let testParse = (t: Zora.t, p, str, ~expect=?) => {
  switch P.runParser(p, str) {
  | Ok((res, "")) => expect->Option.map(expect => t->equal(res, expect))->ignore
  | Ok((_, rem)) => t->fail(~msg=`failed to consume remaining input: ${rem}`)
  | Error(e) => t->fail(~msg=`parse failed`)
  }
}

zoraBlock("parse", t => {
  t->testParse(P.string("a"), "a", ~expect="a")
  t->testParse(P.token("a"), "a   \n\r", ~expect="a")
  t->testParse(P.string("a")->P.or(P.string("b")), "b", ~expect="b")
  let abs =
    P.takeWhile(s => s->String.startsWith("a"))->P.bind(res => P.string("b")->P.map(b => (res, b)))
  t->testParse(abs, "aaaab", ~expect=("aaaa", "b"))
})
