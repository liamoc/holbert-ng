let newline = "\n"
let mapMapValues = (m: Map.t<'a, 'b>, f: 'b => 'c) => {
  let nu = Map.make()
  m->Map.forEachWithKey((v, k) => {
    nu->Map.set(k, f(v))
  })
  nu
}
let withKey: ('props, int) => 'props = %raw(`(props, key) => ({...props, key})`)

let arrayWithIndex = (arr: array<React.element>) => {
  React.array(arr->Array.mapWithIndex((m, i) => <span key={String.make(i)}> m </span>))
}
exception TODO(string)
exception Unreachable(string)
exception Err(string)
