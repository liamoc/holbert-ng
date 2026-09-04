

@react.component
let make = (~label: string, ~onConfirm: string => result<unit, string>) => {
  let (isEditing, setIsEditing) = React.useState(() => false)
  let (text, setText) = React.useState(() => label)
  let (tempText, setTempText) = React.useState(() => label)
  let (errorMsg, setErrorMsg) = React.useState(() => None)

  let handleConfirm = _evt => {
    switch onConfirm(tempText) {
    | Ok() =>
      setText(_ => tempText)
      setErrorMsg(_ => None)
      setIsEditing(_ => false)
    | Error(msg) =>
      // Revert text and display error popup
      setTempText(_ => text)
      setErrorMsg(_ => Some(msg))
    }
  }

  let handleCancel = _evt => {
    setTempText(_ => text)
    setErrorMsg(_ => None)
    setIsEditing(_ => false)
  }

  let dismissError = _evt => {
    setErrorMsg(_ => None)
  }

  <div className="editable-container">
    {if isEditing {
      <div className="edit-wrapper">
        <div className="controls-row">
          <input
            type_="text"
            className="auto-input"
            value={tempText}
            size={Math.Int.max(1, String.length(tempText))}
            onChange={evt => setTempText(_ => ReactEvent.Form.target(evt)["value"])}
            onKeyDown={evt => {
              let key = ReactEvent.Keyboard.key(evt)
              if key == "Enter" { handleConfirm(evt) }
              else if key == "Escape" { handleCancel(evt) }
            }}
            autoFocus=true
          />
          <span
            className="editor-button button-icon button-icon-blue typcn typcn-tick"
           onClick={handleConfirm}> </span>
           <span
             className="editor-button button-icon button-icon-red typcn typcn-times"
          onClick={handleCancel}> </span>
        </div>

        {switch errorMsg {
        | Some(msg) =>
          <div className="error-box">
            <span> {React.string(msg)} </span>
            <span
              className="editor-button button-icon button-icon-grey typcn typcn-times"
             onClick={dismissError}>
            </span>
          </div>
        | None => React.null
        }}
      </div>
    } else {
      <span
        className="label-span"
        onClick={_ => {
          setTempText(_ => text)
          setErrorMsg(_ => None)
          setIsEditing(_ => true)
        }}>
        <IdentifierView identifier=text />
      </span>
    }}
  </div>
}
