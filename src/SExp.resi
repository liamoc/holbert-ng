// TODO: i only added the with type t = string later.
// may remove need for helpers down bottom
module ConstSymbol: SExpFunc.SYMBOL with type t = string

include module type of SExpFunc.Make(ConstSymbol)
let symbol: string => t
let pSymbol: string => ConstSymbol.t
let getSymbol: ConstSymbol.t => string
