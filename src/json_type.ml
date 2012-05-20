(* Ohm is © 2012 Victor Nicollet *)

type t =
  | Null
  | Array of t list
  | Object of (string * t) list
  | Float of float
  | Int of int
  | Bool of bool
  | String of string
      
exception Error of string
    
