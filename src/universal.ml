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

let nothrow_or default expr callback = 
  let what = try Some (Lazy.force expr) with _ -> None in
  req_or default what callback 

let true_or default t callback = 
  if t then callback () else default

let ohm_req_or default query callback = 
  ohm query (fun x -> req_or default x callback)

let ohm_ok_or ifbad query callback = 
  ohm query (function 
    | Bad bad -> ifbad bad
    | Ok  ok  -> callback ok)

let opt func value callback = 
  match value with 
    | None -> callback None
    | Some value -> func value (fun result -> callback (Some result)) 
