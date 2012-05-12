(* Ohm is © 2012 Victor Nicollet *)

(** Internationalization and translation. 

    This module is separated into two distinct sections. The first
    section is related to how the environment should be set up in order
    to translate things at will, such as how to add or extract the 
    internationalization function from the context.

    The second section is a set of utility functions used when
    building internationalization functions. 
*)

(** {2 Context management} *)

(** A source maps keys to the corresponding translated strings. *)
type 'key source = 'key -> string

(** A source may be found within any context as a member named adlib. 
    Inherit your context from this class to handle this. *)
class ['key] ctx : 'key source -> object
  method adlib : 'key source
end

(** {2 Utilities} *)

(** Alias for [Printf.sprintf]. *)
val (!!) : ('a, unit, string) format -> 'a
  
(** Ignores second argument, returns first argument. *)
val const : 'a -> 'b -> 'a

(** {2 Gendered translations} *)

(** The gender may be male, female or unknown. *)
type gender = [`m | `f] option

(** A gendered text. [gendered g "male" "female" "unknown"] returns the
    translation that corresponds to the provided gender. 
*)
val gendered : 'a -> 'a -> 'a -> gender -> 'a

(** A gendered text. [macho "male" "female"] is equivalent to 
    [gendered "male" "female" "male"] (that is, the male gender is
    assumed when no gender is known). 
*)
val macho : 'a -> 'a -> gender -> 'a

(** {2 Cardinal translations} *)

(** English-style plurals: a special case for zero, a special case
    for 1, and a generic case for numbers over 20.

    [n "no bananas" "one banana" (!! "%d bananas") n] 
*)
val card : 'a -> 'a -> (int -> 'a) -> int -> 'a
