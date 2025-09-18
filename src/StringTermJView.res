module TermView = StringTermView
type props = {
  judgment: StringTermJudgment.t,
  scope: array<string>,
}
let make = ({judgment: (term, j), scope}) => {
  <span className="term-compound">
    <StringTermView term scope />
    {React.string(" - ")}
    <SExpView term=j scope />
  </span>
}
