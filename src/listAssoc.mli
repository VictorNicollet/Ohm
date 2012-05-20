(* Ohm is © 2011 Victor Nicollet *)

type ('key,'value) t = ('key * 'value) list

val get : ('key, 'value) t -> 'key -> 'value

val try_get : ('key, 'value) t -> 'key -> 'value option

val set : 'key -> 'value -> ('key,'value) t -> ('key,'value) t

val replace : 'key -> 'value -> ('key,'value) t -> ('key,'value) t

val unset : 'key -> ('key,'value) t -> ('key,'value) t

val pop : 'key -> ('key,'value) t -> ('key,'value) t * 'value 

val try_pop : 'key -> ('key,'value) t -> ('key,'value) t * 'value option

val move : ?after:'key -> 'key -> ('key,'value) t -> ('key,'value) t

val keys : ('key,'value) t -> 'key list

val values : ('key,'value) t -> 'value list

val map : 'key -> ('value -> 'value) -> ('key, 'value) t -> ('key, 'value) t

val group : ('key * 'value) list -> ('key, 'value list) t

val group_stable : ('key * 'value) list -> ('key, 'value list) t

val of_json : (string -> 'key) -> (Json.t -> 'value) -> Json.t -> ('key,'value) t 

val to_json : ('key -> string) -> ('value -> Json.t) -> ('key,'value) t -> Json.t
