let newline = "\n"
let withKey: ('props, int) => 'props = %raw(`(props, key) => ({...props, key})`)

let arrayWithIndex = (arr: array<React.element>) => {
  React.array(arr->Array.mapWithIndex((m, i) => <span key={String.make(i)}> m </span>))
}
exception TODO(string)
exception Unreachable(string)
exception Err(string)
