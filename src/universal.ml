(* Ohm is © 2011 Victor Nicollet *)

open BatPervasives

let bind f g = f g 

let ($) f x = f x

let ohm t f = Run.bind f t
let ohmctx lens f = Run.bind f (Run.map lens Run.context)

let fork a f = let b = f () in Run.fork a b

let return = Run.return

let req_or default opt callback = match opt with 
  | Some value -> callback value
  | None -> default

let true_or default t callback = 
  if t then callback () else default

let ohm_req_or default query callback = 
  ohm query (fun x -> req_or default x callback)

let optional value func callback = 
  match value with 
    | None -> callback None
    | Some value -> func value (fun result -> callback (Some result)) 
