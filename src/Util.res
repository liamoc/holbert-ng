let newline = "\n"
let withKey: ('props, int) => 'props = %raw(`(props, key) => ({...props, key})`)

let arrayWithIndex = (arr: array<React.element>) => {
  React.array(arr->Array.mapWithIndex((m, i) => <span key={String.make(i)}> m </span>))
}
exception Unreachable(string)
exception Err(string)
let mustFindIndex = (arr, f) => {
  switch Array.findIndex(arr, f) {
  | -1 => raise(Unreachable("Element not found"))
  | i => i
  }
}
