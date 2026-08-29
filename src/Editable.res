open Component

module TextArea = (Underlying: COMPONENT) => {
  module Ports = Underlying.Ports
  type props = {
    content: result<Underlying.state, (string, string)>,
    imports: Ports.t,
    onChange: (result<Underlying.state, (string, string)>, ~exports: Ports.t=?) => unit,
    reset: unit => unit,
  }
  type state = result<Underlying.state, (string, string)>

  let serialise = state =>
    switch state {
    | Ok(s) => Underlying.serialise(s)
    | Error((_err, str)) => str
    }
  let deserialise = (str: string, ~imports: Ports.t) => {
    switch Underlying.deserialise(str, ~imports) {
    | Ok((s, e)) => Ok(Ok(s), e)
    | Error(e) => Ok(Error(e, str), Ports.empty)
    }
  }
  let make = props => {
    let (editing, setEditing) = React.useState(_ => false)
    let (text, setText) = React.useState(_ => serialise(props.content))
    let onTextChange = (ev: JsxEvent.Form.t) => {
      let target = JsxEvent.Form.target(ev)
      let value: string = target["value"]
      setText(_ => value)
    }
    let done = _ => {
      switch deserialise(text, ~imports=props.imports) {
      | Ok((st, ex)) => props.onChange(st, ~exports=ex)
      | Error(_e) => Console.log("Impossible happened")
      }
      setEditing(_ => false)
    }
    if editing {
      <div>
        <textarea className="editor-textArea" value={text} onChange={onTextChange} />
        <div className="editor-controls">
          <span
            className="editor-button button-icon button-icon-blue typcn typcn-tick" onClick={done}
          />
        </div>
      </div>
    } else {
      switch props.content {
      | Ok(us) =>
        <div>
          <Underlying
            content={us}
            imports={props.imports}
            reset={props.reset}
            /* onLoad={(~exports, ~string=?) =>
             props.onLoad(~exports, ~string=string->Option.getOr(Underlying.serialise(us)))} */
            onChange={(state, ~exports=?) => {
              props.onChange(Ok(state), ~exports?)
            }}
          />
          <div className="editor-controls">
            <span
              className="editor-button button-icon button-icon-blue typcn typcn-edit"
              onClick={_ => {
                setText(_ => serialise(props.content))
                setEditing(_ => true)
              }}
            />
            <span
              className="editor-button button-icon button-icon-blue typcn typcn-arrow-loop"
              onClick={_ => {
                props.reset()
              }}
            />
          </div>
        </div>
      | Error((err, _str)) =>
        <div>
          <div className="error"> {React.string(err)} </div>
          <div className="editor-controls">
            <span
              className="editor-button button-icon button-icon-blue typcn typcn-edit"
              onClick={_ => {
                setText(_ => serialise(props.content))
                setEditing(_ => true)
              }}
            />
            <span
              className="editor-button button-icon button-icon-blue typcn typcn-arrow-loop"
              onClick={_ => {
                props.reset()
              }}
            />
          </div>
        </div>
      }
    }
  }
}
