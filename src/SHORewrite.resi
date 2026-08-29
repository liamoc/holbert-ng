type direction = Forward | Backward
type t<'a> = {
  ruleName: string,
  direction: direction,
  path: SHOTerm.path,
  values: array<SHOTerm.t>,
  newGoal: 'a,
  subgoals: array<'a>,
}
include Method.PROOF_METHOD with 
  module Term := SHOTerm and 
  module Judgment := SHOTerm and
  type t<'a> := t<'a>


let updateAtKey : (t<'a>, int, 'a => 'a) => t<'a>

let updateGoal : (t<'a>, 'a => 'a) => t<'a>