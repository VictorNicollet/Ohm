(* Ohm is © 2012 Victor Nicollet *)

type 'key source = 'key -> string

class ['key] ctx (source : 'key source) = object
  val adlib = source
  method adlib = adlib
end

let get key = 
  Run.map (fun ctx -> ctx # adlib key) Run.context 

let write key = 
  Run.map (fun ctx -> Html.esc (ctx # adlib key)) Run.context 

let (!!) fmt = Printf.sprintf fmt 
let const x _ = x

type gender = [`m | `f] option

let gendered male female other = function
  | Some `m -> male
  | Some `f -> female
  | None    -> other

let macho male female = function
  | Some `f -> female
  | _       -> male

let card zero one many = function
  | 0 -> zero
  | 1 -> one
  | n -> many n 
