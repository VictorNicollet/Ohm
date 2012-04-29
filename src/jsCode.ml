(* Ohm is © 2011 Victor Nicollet *)

open Util
open BatPervasives

type call = {
  name : string ;
  args : Json_type.t list 
}

type t = Leaf of call | Node of t list

let empty = Node []

let make ~name ~args = Leaf ({ name = name ; args = args }) 

let seq list = Node list

let list_of_tree list = 
  let rec aux list = function 
    | Leaf c -> c :: list
    | Node [] -> list
    | Node (h::t) -> aux (aux list (Node t)) h
  in
  aux [] list


let script_of_call call = 
  call.name 
  ^ "(" 
  ^ String.concat ", " 
    (List.map (Json_io.string_of_json ~recursive:true ~compact:true) call.args)
  ^ ")"
    
let to_script t =
  list_of_tree t
  |> List.map script_of_call    
  |> String.concat " ; "
    
let event_of_call call = 
  call.name 
  ^ ".call(" 
  ^ String.concat ", " 
    ("this" :: List.map (Json_io.string_of_json ~recursive:true ~compact:true) call.args)
  ^ ")"

let to_event t = 
  list_of_tree t
  |> List.map event_of_call    
  |> String.concat " ; "

let to_json tree = 
  Json_type.Build.list begin fun call ->
    Json_type.Build.array ( Json_type.Build.string call.name :: call.args )
  end (list_of_tree tree)
	
	     
