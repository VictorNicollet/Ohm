(* Ohm is © 2012 Victor Nicollet *)

(* Parsers use a reference to store an already-parsed version of some JSON, in order
   to eliminate multiple parsing of the same data. *) 

type cache = (unit -> unit) ref
    
type 'a t  = { 
  mutable store : 'a option ;
          parse : Json_type.t -> 'a option
} 
    
let make parse = { store = None ; parse } 
  
let read (t:'a t) json cache = 
  t.store <- None ;
  (!cache) () ;
  match t.store with Some value -> Some value | None -> 
    match t.parse json with None -> None | Some value -> 
      cache := (fun () -> t.store <- Some value) ; Some value
	
let cache () = ref (fun () -> ())
  
