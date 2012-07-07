(* Ohm is © 2011 Victor Nicollet *)

open Util

type 'a t = {
  to_json : 'a -> Json_type.t ;
  of_json : Json_type.t -> 'a option
}

type 'a fmt = 'a t

let optional fmt = { 
  to_json = (function 
    | Some x -> fmt.to_json x
    | None   -> Json.Null) ;
  of_json = (function 
    | Json.Null -> Some None
    | other -> match fmt.of_json other with 
	| Some x -> Some (Some x)
	| None   -> None)
}

let protect ?save f v = 
  let fail error =     
    log "Format: `%s` on: %s" error (logjson v) ;
    None
  in
  
  try Some (f v)
  with Json.Error error ->
    match save with None -> fail error | Some g -> 
      try Some (f (g v)) with _ -> fail error             

let real_value = function
  | Json.Object []
  | Json.Array  []
  | Json.String ""
  | Json.Null      -> None
  | other               -> Some other

module type FMT = sig
  type t 
  val of_json : Json.t -> t
  val to_json : t -> Json.t
  val of_json_safe : Json.t -> t option
  val of_json_string_safe : string -> t option
  val to_json_string : t -> string
  val fmt     : t fmt
end

module type READ_FMT = sig
  type t 
  val of_json : Json.t -> t
  val of_json_safe : Json.t -> t option
  val of_json_string_safe : string -> t option
end

module Extend = functor (Type : sig
  type t 
  val t_of_json : Json.t -> t
  val json_of_t : t -> Json.t
end) -> struct

  let of_json = Type.t_of_json
  let to_json = Type.json_of_t
  let of_json_safe = protect of_json

  let of_json_string_safe str = 
    try of_json_safe (Json.unserialize str)
    with _ -> None
  
  let to_json_string t = 
    Json.serialize (to_json t)
            
  let fmt     = { to_json = to_json ; of_json = of_json_safe }

end

module ReadExtend = functor (Type : sig
  type t 
  val t_of_json : Json.t -> t
end) -> struct

  let of_json = Type.t_of_json

  let of_json_safe = protect of_json

  let of_json_string_safe str = 
    try of_json_safe (Json.unserialize str)
    with _ -> None
  
end

module Make = functor (Type : sig 
  type t 
  val t_of_json : Json.t -> t
  val json_of_t : t -> Json.t
end) -> struct
  type t      = Type.t
  include Extend(Type)
end

module String = Make(struct
  type json t = string
end)

module Int = Make(struct
  type json t = int
end)

module Bool = Make(struct
  type json t = bool
end)

module Unit = Make(struct
  type json t = unit
end)

module Float = Make(struct
  type json t = float
end)

module Json = Make(struct
  type t = Json.t
  let t_of_json x = x
  let json_of_t x = x
end)
