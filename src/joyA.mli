(* Ohm is © 2011 Victor Nicollet *)

module Node : Fmt.FMT

type t = Node.t
type alternative
type field

val string :
     ?editor:[`line|`area]
  -> ?validators:[`min of int|`max of int] list
  -> ?autocomplete:string
  -> unit
  -> t

val array :
     ?sortable:bool
  -> ?validators:[`min of int|`max of int] list
  -> t
  -> t

val dict : t -> t 

val bool : t
  
val label : string -> t -> t

val optional : t -> t

val field :
     string
  -> ?label:string
  -> t
  -> field

val obj : field list -> t

val alternative : 
     ?content:t
  -> ?label:string
  -> string
  -> alternative

val variant : alternative list -> t

val tuple : (string * t) list -> t

module type FMT = sig
  val edit : t
  include Fmt.FMT
end

module Make : functor (Type : sig
  val edit : t
  type t
  val t_of_json : Json_type.t -> t
  val json_of_t : t -> Json_type.t
end) -> FMT with type t = Type.t
