(* Jogging is © 2011 Victor Nicollet *)

type role = [ `Bot | `Web | `Put | `Reset ]

val role : role

val get_binary_contents : string -> string option 
val get_view_contents : string -> string option
val get_resource_contents : string -> string option

val urlencode : string -> string

val log : ('a,unit,string,unit) format4 -> 'a
val logreq : ('a,unit,string,unit) format4 -> 'a
val logjson : Json_type.t -> string

val uniq : unit -> string

val dec_of_hex_char : char -> int

val base62 : int -> int -> string
val base62_of_int : int -> string
val base62_to_base34 : string -> string

val last : 'a list -> 'a option
val first : 'a list -> 'a option

val string_of_time : float -> string

val utf8 : string -> string option

val sha1_hmac : (string * string) -> string -> string

val setdiff : ('a -> 'b -> int) -> 'a list -> 'b list -> 'a list
val setand  : ('a -> 'b -> int) -> 'a list -> 'b list -> 'a list

val memoize : ('a -> 'b) -> 'a -> 'b

val fold_accents : string -> string
val fold_all : string -> string

val number : 'a list -> (int * 'a) list

val clip : int -> string -> string

val next_string : string -> string
