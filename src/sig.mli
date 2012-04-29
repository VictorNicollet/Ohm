(* Ohm is © 2011 Victor Nicollet *)

type ('input,'output) listener = 'input -> 'output

type ('input,'output) channel 

val make : ('output list -> 'output) -> ('input,'output) listener * ('input,'output) channel 

val listen : ('input,'output) channel -> ('input,'output) listener -> unit 
