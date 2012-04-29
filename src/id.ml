(* Ohm is © 2011 Victor Nicollet *)

open BatPervasives

include Fmt.Make(struct type json t = string end)

let compare = compare

let to_id = identity
let of_id = identity
let of_string = identity
let to_string = identity	
let str = identity
let sel id = "#"^id

let gen () = Util.uniq ()

let length   = 11
let smallest = ""
let largest  = String.make length 'z'

let next_char = function
  | '9' -> 'A'
  | 'Z' -> 'a'
  | 'z' -> '0'
  |  c  -> Char.chr (1 + Char.code c)

let next s = 
  let s = String.copy s and i = ref (length-1) in
  while !i >= 0 do
    s.[!i] <- next_char s.[!i] ;
    if s.[!i] = '0' then decr i else i := (-1)
  done ;
  s

module Phantom = struct

  type 'nature id  = t
  type t = [`Unknown] id

  let compare = compare

  let of_id id = id
  let to_id id = id

  let to_string = str 
  let of_string = of_string 

  let gen () = gen ()
  let smallest = smallest
  let largest  = largest
  let next  id = next id
  let decay id = id

  let to_json = to_json
  let of_json = of_json 
  let of_json_safe = of_json_safe
  let of_json_string_safe = of_json_string_safe
  let to_json_string = to_json_string

  let fmt     = fmt

end
  
module type PHANTOM = sig

  type 'relation id

  val compare : 'relation id -> 'relation id -> int

  val of_id : t       -> [`Unknown] id
  val to_id : 'any id -> t

  include Fmt.FMT with type t = [`Unknown] id

  val fmt : 'a id Fmt.t

  val gen : unit -> t
  val smallest : t
  val largest : t
  val next : 'a id -> 'a id

  val to_string : 'any id -> string
  val of_string : string -> t

  val decay : 'any id -> t

end
