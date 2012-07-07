(* Ohm is © 2011 Victor Nicollet *)

type 'a t = {
  to_json : 'a -> Json.t ;
  of_json : Json.t -> 'a option
}

type 'a fmt = 'a t

val optional : 'a fmt -> 'a option fmt

val protect : ?save:(Json.t -> Json.t) -> (Json.t -> 'a) -> Json.t -> 'a option

val real_value : Json.t -> Json.t option

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

module Make : functor (Type : sig
  type t
  val t_of_json : Json.t -> t
  val json_of_t : t -> Json.t
end ) -> FMT with type t = Type.t

module Extend : functor (Type : sig 
  type t 
  val t_of_json : Json.t -> t
  val json_of_t : t -> Json.t
end ) -> sig
  open Type
  val of_json : Json.t -> t
  val to_json : t -> Json.t
  val of_json_safe : Json.t -> t option
  val of_json_string_safe : string -> t option
  val to_json_string : t -> string
  val fmt     : t fmt  
end

module ReadExtend : functor (Type : sig
  type t
  val t_of_json : Json.t -> t
end) -> sig
  open Type
  val of_json : Json.t -> t
  val of_json_safe : Json.t -> t option
  val of_json_string_safe : string -> t option 
end

module Unit   : FMT with type t = unit
module Float  : FMT with type t = float
module String : FMT with type t = string
module Int    : FMT with type t = int
module Bool   : FMT with type t = bool
module Json   : FMT with type t = Json.t

