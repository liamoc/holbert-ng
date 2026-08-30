open Signatures

module Make = (Term: TERM, TermView: TERM_VIEW with module Term := Term) => {
  type props = {scope: array<Term.meta>, editable: option<array<Term.meta> => unit>}
  @react.componentWithProps
  let make = props => {
    let arr =
      props.scope->Array.mapWithIndex((m, i) =>
        <React.Fragment key={String.make(i)}> 
        {
          switch props.editable {
          | None => TermView.makeMeta(m)
          | Some(call) => TermView.makeEditableMeta(m,~onChange= m' => {
              call(Util.updateAtIndex(props.scope,i,m'))
            })
          }
        } 
        </React.Fragment>
      )
    Array.reverse(arr)
    if Array.length(arr) > 0 {
      <span className="rule-binders"> {React.array(arr)} </span>
    } else {
      React.null
    }
  }
}
