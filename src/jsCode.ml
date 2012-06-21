(* Ohm is © 2011 Victor Nicollet *)

open Util
open BatPervasives

type call = {
  name : string ;
  args : Json_type.t list 
}

module Endpoint = struct

  type t = Url of string | Js of call

  let of_url url = Url url
  let of_js ~name ~args = Js { name ; args }

  let to_json = function
    | Url url -> Json.String url
    | Js  js  -> Json.Array ( Json.String js.name :: js.args )
      
end

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
    (List.map Json.serialize call.args)
  ^ ")"
    
let to_script t =
  list_of_tree t
  |> List.map script_of_call    
  |> String.concat " ; "
    
let event_of_call call = 
  call.name 
  ^ ".call(" 
  ^ String.concat ", " 
    ("this" :: List.map Json.serialize call.args)
  ^ ")"

let to_event t = 
  list_of_tree t
  |> List.map event_of_call    
  |> String.concat " ; "

let to_json tree = 
  Json.of_list begin fun call ->
    Json.Array ( Json.String call.name :: call.args )
  end (list_of_tree tree)
	
	     
