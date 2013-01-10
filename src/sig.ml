(* Ohm is © 2013 Victor Nicollet *)

type ('a,'b) listener = 'a -> 'b

type ('a,'b) channel  = ('a,'b) listener -> unit 

let listen channel listener = channel listener

let make collapse = 
  let callbacks = ref [] in
  let channel f = callbacks := f :: !callbacks in
  let call x = collapse (List.map (fun f -> f x) !callbacks) in
  call, channel

module Std = struct

  module Web = struct

    let init_, init = make ignore

  end

  module Put = struct
      
    let once_, once = make ignore

  end

  module Bot = struct

    let init_, init = make ignore
    let tick_, tick = make ignore

  end

end
