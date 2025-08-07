module TermView = HOTermView
type props = {
  judgment: HOTerm.t,
  scope: array<string>,
}
let make = ({judgment, scope}) => HOTermView.make({term: judgment, scope})
