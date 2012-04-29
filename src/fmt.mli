(* Jogging is © 2011 Victor Nicollet *)

type 'a t = {
  to_json : 'a -> Json_type.t ;
  of_json : Json_type.t -> 'a option
}

type 'a fmt = 'a t

val fix : (string * Json_type.t -> string * Json_type.t) -> Json_type.t -> Json_type.t
val protect : ?save:(Json_type.t -> Json_type.t) -> (Json_type.t -> 'a) -> Json_type.t -> 'a option

val real_value : Json_type.t -> Json_type.t option

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

module Make : functor (Type : sig
  type t
  val t_of_json : Json_type.t -> t
  val json_of_t : t -> Json_type.t
end ) -> FMT with type t = Type.t

module Extend : functor (Type : sig 
  type t 
  val t_of_json : Json_type.t -> t
  val json_of_t : t -> Json_type.t
end ) -> sig
  open Type
  val of_json : Json_type.t -> t
  val to_json : t -> Json_type.t
  val of_json_safe : Json_type.t -> t option
  val of_json_string_safe : string -> t option
  val to_json_string : t -> string
  val fmt     : t fmt  
end

module ReadExtend : functor (Type : sig
  type t
  val t_of_json : Json_type.t -> t
end) -> sig
  open Type
  val of_json : Json_type.t -> t
  val of_json_safe : Json_type.t -> t option
  val of_json_string_safe : string -> t option 
end

module Unit   : FMT with type t = unit
module Float  : FMT with type t = float
module String : FMT with type t = string
module Int    : FMT with type t = int
module Json   : FMT with type t = Json_type.t
module Bool   : FMT with type t = bool

