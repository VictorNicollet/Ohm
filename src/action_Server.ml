(* Ohm is © 2012 Victor Nicollet *)

open Util
open BatPervasives
module BS = BatString

open Action_Common

class type ['param] server = object
  method protocol      : 'param -> [`HTTP|`HTTPS]
  method domain        : 'param -> string
  method port          : 'param -> int
  method cookie_domain : 'param -> string option
  method matches       : [`HTTP|`HTTPS] -> string -> int -> 'param option
end

let server_root server param = 
  let protocol = server # protocol param in
  String.concat "" 
    ( (match protocol with `HTTP -> "http://" | `HTTPS -> "https://")
      :: (server # domain param) 
      :: (match protocol, server # port param with 
	| `HTTPS, 443 | `HTTP, 80 -> []
	| _, port -> [ ":" ; string_of_int port ]))
