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

let id name = put [
  ml  name, "include Ohm.Id.Phantom\n" ;
  mli name, "include Ohm.Id.PHANTOM\n" ;
]


let one = function
  | name when name -- "I[A-Za-z0-9]*" -> id name
  | name -> print_endline (Printf.sprintf "Don't know what to do with %S" name)

let make args = List.iter one args
