(* Ohm is © 2011 Victor Nicollet *)

(** If the input string is or contains an e-mail address, return a canonical 
    version of it (with spaces and non-address characters stripped, and in
    lowercase). *)
val canonical : string -> string
