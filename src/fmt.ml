(* Ohm is © 2011 Victor Nicollet *)

open Util

type 'a t = {
  to_json : 'a -> Json_type.t ;
  of_json : Json_type.t -> 'a option
}

type 'a fmt = 'a t

let protect ?save f v = 
  let fail error =     
    log "Format: `%s` on: %s" error (logjson v) ;
    None
  in
  
  try Some (f v)
  with Json_type.Json_error error ->
    match save with None -> fail error | Some g -> 
      try Some (f (g v)) with _ -> fail error             

(* Float hack : CouchDB converts 1000000000.0 to 10000000000 and OCaml tries to 
   read it back as an integer. *)
let hackfloat flt = 
  let flt = 
    if abs_float flt > float_of_int max_int && min (ceil flt -. flt) (flt -. floor flt) < 0.01 then
      flt +. 0.02
    else
      flt
  in Json_type.Build.float flt

let real_value = function
  | Json_type.Object []
  | Json_type.Array  []
  | Json_type.String ""
  | Json_type.Null      -> None
  | other               -> Some other

module type FMT = sig
  type t 
  val of_json : Json_type.t -> t
  val to_json : t -> Json_type.t
  val of_json_safe : Json_type.t -> t option
  val of_json_string_safe : string -> t option
  val to_json_string : t -> string
  val fmt     : t fmt
end

module type READ_FMT = sig
  type t 
  val of_json : Json_type.t -> t
  val of_json_safe : Json_type.t -> t option
  val of_json_string_safe : string -> t option
end

module Extend = functor (Type : sig
  type t 
  val t_of_json : Json_type.t -> t
  val json_of_t : t -> Json_type.t
end) -> struct

  let of_json = Type.t_of_json
  let to_json = Type.json_of_t
  let of_json_safe = protect of_json

  let of_json_string_safe str = 
    try of_json_safe (Json_io.json_of_string ~recursive:true str)
    with _ -> None
  
  let to_json_string t = 
    Json_io.string_of_json ~compact:true ~recursive:true (to_json t)
            
  let fmt     = { to_json = to_json ; of_json = of_json_safe }

end

module ReadExtend = functor (Type : sig
  type t 
  val t_of_json : Json_type.t -> t
end) -> struct

  let of_json = Type.t_of_json

  let of_json_safe = protect of_json

  let of_json_string_safe str = 
    try of_json_safe (Json_io.json_of_string ~recursive:true str)
    with _ -> None
  
end

module Make = functor (Type : sig 
  type t 
  val t_of_json : Json_type.t -> t
  val json_of_t : t -> Json_type.t
end) -> struct
  type t      = Type.t
  include Extend(Type)
end

module Json = Make(struct 
  type t = Json_type.t
  let name = "Json_type.t"
  let t_of_json x = x
  let json_of_t x = x
end)

module String = Make(struct
  type t = string
  let name = "string"
  let t_of_json = Json_type.Browse.string
  let json_of_t = Json_type.Build.string
end)

module Int = Make(struct
  type t = int
  let name = "int"
  let t_of_json = Json_type.Browse.int
  let json_of_t = Json_type.Build.int
end)

module Bool = Make(struct
  type t = bool
  let name = "int"
  let t_of_json = Json_type.Browse.bool
  let json_of_t = Json_type.Build.bool 
end)

module Unit = Make(struct
  type t = unit
  let name = "unit"
  let t_of_json = fun _ -> ()
  let json_of_t = fun () -> Json_type.Build.null
end)

module Float = Make(struct
  type t = float
  let name = "float"
  let t_of_json = Json_type.Browse.number
  let json_of_t = hackfloat 
end)

let fix f = function
  | Json_type.Object list -> Json_type.Object (List.map f list)
  | other -> other
