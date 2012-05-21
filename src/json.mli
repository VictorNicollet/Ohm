(* Ohm is © 2012 Victor Nicollet *)

type t = Json_type.t = 
  | Null
  | Array of t list
  | Object of (string * t) list
  | Float of float
  | Int of int
  | Bool of bool
  | String of string

exception Error of string 

val to_json : t -> t
val of_json : t -> t

val debug : t -> string

val unserialize : string -> t
val serialize   : t -> string

val of_assoc  : (string * t) list -> t 
val of_int    : int -> t 
val of_float  : float -> t
val of_string : string -> t 
val of_bool   : bool -> t
val of_array  : t list -> t
val of_opt    : ('a -> t) -> 'a option -> t 
val of_list   : ('a -> t) -> 'a list -> t 

val to_object : (opt:(string -> t option) -> req:(string -> t) -> 'a) -> t -> 'a 
val to_list   : (t -> 'a) -> t -> 'a list
val to_int    : t -> int
val to_float  : t -> float
val to_bool   : t -> bool
val to_string : t -> string
val to_opt    : (t -> 'a) -> t -> 'a option
val to_array  : t -> t list
val to_assoc  : t -> (string * t) list 

val parse     : (t -> 'a) -> t -> ('a,exn) BatStd.result 
 
