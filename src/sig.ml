(* Ohm is © 2011 Victor Nicollet *)

type ('a,'b) listener = 'a -> 'b

type ('a,'b) channel  = ('a,'b) listener -> unit 

let listen channel listener = channel listener

let make collapse = 
  let callbacks = ref [] in
  let channel f = callbacks := f :: !callbacks in
  let call x = collapse (List.map (fun f -> f x) !callbacks) in
  call, channel
