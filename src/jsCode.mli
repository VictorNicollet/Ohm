(* Ohm is © 2011 Victor Nicollet *)

type call = {
  name : string ;
  args : Json_type.t list 
}

type t = Leaf of call | Node of t list

val empty : t 

val make : name:string -> args:Json_type.t list -> t

val seq : t list -> t 

val list_of_tree : t -> call list

val to_string : t -> string
