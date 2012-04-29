(* Ohm is © 2011 Victor Nicollet *)

type path = 
  [ `Log
  | `Templates
  | `Resources 
  ]

exception Locked of path * string

val root : string

val get : path -> string

val set : path -> string -> unit

val lock : path -> string
