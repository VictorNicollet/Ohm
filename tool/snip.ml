(* Ohm is © 2013 Victor Nicollet *)

open BatPervasives

open Common

let (--) name regex = 
  Str.string_match (Str.regexp regex) name 0 
  && Str.match_end () = String.length name 

let ml name = 
  "ocaml/" ^ String.uncapitalize name ^ ".ml"

let mli name = 
  "ocaml/" ^ String.uncapitalize name ^ ".mli"

let put list = List.iter (fun (file,contents) ->
  let path  = Filename.concat Path.root file in
  if file_exists path && readfile path <> contents then
    error "Could not write file" (Printf.sprintf "File %s already exists." path) ;
  if putfile path contents then
    print_endline (">> " ^ file) 
) list

(* Actual generation ---------------------------------------------------------------------- *)

let id name = put [
  ml  name, "include Ohm.Id.Phantom\n" ;
  mli name, "include Ohm.Id.PHANTOM\n" ;
]

let model name = 
  let short = BatString.lchop name in 
  id ("I" ^ short) ; 
  put [

    ml name, Printf.sprintf "open Ohm
open Ohm.Universal

module Data = struct
  module T = struct
    type json t = {
    }
  end
  include T
  include Fmt.Extend(T)
end

include CouchDB.Convenience.Table
  (struct let db = O.db %S end)(I%s)(Data)
" 
      (String.lowercase short) (* DB name *)
      short ; 

    mli name, Printf.sprintf "module Data : sig 
  type t = {
  }
end

module Tbl : Ohm.CouchDB.TABLE 
  with type elt = Data.t 
  and  type id  = I%s.t

module Design : Ohm.CouchDB.DESIGN
"
      short
]

(* Entry pointration ---------------------------------------------------------------------- *)

let one = function
  | name when name -- "I[A-Za-z0-9]*" -> id name
  | name when name -- "M[A-Za-z0-9]*" -> model name 
  | name -> print_endline (Printf.sprintf "Don't know what to do with %S" name)

let make args = List.iter one args
