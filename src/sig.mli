(* Ohm is © 2013 Victor Nicollet *)

type ('input,'output) listener = 'input -> 'output

type ('input,'output) channel 

val make : ('output list -> 'output) -> ('input,'output) listener * ('input,'output) channel 

val listen : ('input,'output) channel -> ('input,'output) listener -> unit 

module Std : sig

  module Web : sig

    val init  : (unit,unit) channel
    val init_ : (unit,unit) listener

  end

  module Put : sig
      
    val once  : (unit,unit) channel
    val once_ : (unit,unit) listener

  end

  module Bot : sig

    val init  : (unit,unit) channel
    val init_ : (unit,unit) listener
    
    val tick  : (unit,unit) channel
    val tick_ : (unit,unit) listener

  end

end
