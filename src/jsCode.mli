(* Ohm is © 2011 Victor Nicollet *)

type call = {
  name : string ;
  args : Json.t list 
}

module Endpoint : sig

  type t

  val of_url : string -> t
  val of_js  : name:string -> args:Json.t list -> t 

  val to_json : t -> Json.t

end

type t = Leaf of call | Node of t list

val empty : t 

val make : name:string -> args:Json.t list -> t

val seq : t list -> t 

val list_of_tree : t -> call list

val to_script : t -> string
val to_event  : t -> string
val to_json   : t -> Json.t
