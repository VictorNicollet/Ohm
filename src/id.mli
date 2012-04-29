(* Ohm is © 2011 Victor Nicollet *)

(** Unique 11-character string identifiers. *)

(** The type of identifiers. 
    For all practical purposes, this is a string, but it's of an abstract type so that 
    conversions are explicit, thus avoiding an entire category of subtle mistakes. 
*)
type t

val compare : t -> t -> int

(** Convert a JSON value to an identifier. 

    The code below displays {v 00xFv012cFH v}:
    {[ 
let json : Json_type.t = Json_type.Build.string "00xFv012cFH" in
let id : Id.t = Id.of_json json in
print_endline (Id.str id)
    ]}
       
    @raise Json_type.Json_error for JSON values that are not a string.
*)
val of_json : Json_type.t -> t 

(** Convert a JSON value to an identifier, without exceptions.

    Equivalent to {!of_json} but returns [None] instead of raising an exception.
*)
val of_json_safe : Json_type.t -> t option

(** Convert a JSON string to an identifier, without exceptions.

    Parses the provided string as a JSON value, then applies {!on_json_safe}.

    The code below displays {v 00xFv012cFH v}:
    {[ 
let json_string = "\"00xFv012cFH\"" in
let id_opt : Id.t option = Id.of_json_string_safe json in
let Some id = id_opt in 
print_endline (Id.str id)
    ]}
*)
val of_json_string_safe : string -> t option

(** Convert an identifier to a JSON string.

    Applies {!to_json} and serializes the resulting JSON value.

    The code below displays {v "00xFv012cFH" v}:
    {[
let id : Id.t = Id.of_string "00xFv012cFH" in
print_endline (Id.to_json_string id)
    ]}
*)
val to_json_string : t -> string

(** Convert an identifier to a JSON value.

    The code below displays a fresh unique identifier: 
    {[ 
let id : Id.t = Id.gen () in
let json : Json_type.t = Id.to_json id in
print_endline (Json_type.Browse.string json) 
    ]}
*)
val to_json : t -> Json_type.t 

(** Identity, useful when you need to use a CouchDB table without defining a specific
    identifier type. 
*)
val to_id : t -> t

(** Identity, useful when you need to use a CouchDB table without defining a specific
    identifier type. 
*)
val of_id : t -> t

(** Convert a string to an identifier. 

    This is purely a type-casting operation, so that the following is true:
    {[ id == Id.of_string (Id.to_string id) ]}

    It is not necessary for the string to be 11 characters long, but it
    is nonetheless advised.
*)
val of_string : string -> t

(** Convert an identifier to a string. 

    This is purely a type-casting operation, so that the following is true:
    {[ str = Id.to_string (Id.of_string id) ]}
*)
val to_string : t -> string

(** Alias for {!Id.to_string}. *)
val str : t -> string

(** Create the CSS/jQuery selector for an identifier. 
    
    This feature is useful when generating unique element IDs for an HTML document. This function
    prepends a hash (#) character to the identifier, which is the CSS and jQuery syntax for a
    selector that selects that identifier.

    The code below displays {v #my-identifier v}:
    {[
let id : Id.t = Id.of_string "my-identifier" in
print_endline (Id.sel id) 
    ]}
*)
val sel : t -> string

(** Create a new unique identifier using {!Util.uniq}.

    The identifier matches the regular expression {v [a-zA-Z0-9]\{11\} v}. If two identifiers [a] and [b] 
    are generated by the same process in that order, then [a < b]. If they were generated by two different
    processes, [a < b] if [a] was generated at least one second before [b]. 

    There is no risk of collision between identifiers, unless two processes with the same PID each generate
    one identifier at the same second {i and} they have generated the same number of identifiers since they
    started running. 

    The generation function uses [Unix.gettimeofday ()] and [Unix.getpid ()] to compute the current time
    and the PID.
*)
val gen : unit -> t

(** The JSON formatter for identifiers. See module {!Fmt} for more information about JSON formatters. *)
val fmt : t Fmt.t

(** The length of identifiers generated by {!Id.gen}. *)
val length : int

(** The smallest possible ID. Use this for range queries in, for example, CouchDB. *)
val smallest : t

(** The largest possible 11-character ID, as constructed by {!Id.gen}. Use this for range queries in, 
    for example, CouchDB. Do not use if you are building your own identifiers instead of relying
    on {!Id.gen}.
*)
val largest : t

val next : t -> t

(** The type of a strongly typed identifier module. These modules define a new identifier type tagged
    with a type parameter that carries additional information about the identifier. The new identifier
    type has a special type [t] which is an abbreviation for [[`Unknown] id]: a bottom type.

    This module type is intended to be the public interface of all strongly typed identifier modules.
    For implementation, [include] module {!Id.Phantom} below in your module to define
    all the functions in this module type, then add all other necessary functions.
*)
module type PHANTOM = sig

  (** The phantom type. *)
  type 'relation id

  val compare : 'relation id -> 'relation id -> int

  (** Construct a strongly typed identifier from a normal {!Id.t}. *)
  val of_id : t       -> [`Unknown] id

  (** Any strongly typed identifier can be converted back to an identifier. *)
  val to_id : 'any id -> t

  include Fmt.FMT with type t = [`Unknown] id

  val fmt : 'a id Fmt.t

  val gen : unit -> t
  val smallest : t
  val largest : t
  val next : 'a id -> 'a id

  val to_string : 'any id -> string
  val of_string : string -> t

  (** Any identifier can be converted to the bottom type identifier. *)
  val decay : 'any id -> t

end

module Phantom : sig

  type 'relation id = t

  val compare : 'relation id -> 'relation id -> int

  include Fmt.FMT with type t = [`Unknown] id

  val fmt : 'a id Fmt.t

  val of_id : t       -> [`Unknown] id
  val to_id : 'any id -> t

  val gen : unit -> t
  val smallest : t
  val largest : t
  val next : 'a id -> 'a id

  val decay : 'any id -> t

  val to_string : 'any id -> string
  val of_string : string -> t

end