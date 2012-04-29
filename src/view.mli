(* Jogging is © 2011 Victor Nicollet *)
type channel = string -> int -> int -> unit
type 'ctx t = 'ctx -> 'ctx

module Context :
sig

  class type text = 
  object ('ctx)
    method write   : channel
  end

  class type box = 
  object ('ctx)
    method write       : channel
    method add_js_code : JsCode.t -> 'ctx
    method get_js_code : JsCode.t
  end

  val add_js_code  : JsCode.t -> #box t

end

type html = Context.box  t
type text = Context.text t

class channel_writer : Netchannels.rec_out_channel -> Context.text

val write_to_string : Context.text t -> string
val extract : Context.box t -> string * JsCode.t
val str : string       -> #Context.text t
val int : int          -> #Context.text t
val esc : string       -> #Context.text t
val concat : 'ctx t list -> 'ctx t

val foreach : ('arg -> 'ctx t) -> 'arg list -> 'ctx t
val implode : 'ctx t -> ('arg -> 'ctx t) -> 'arg list -> 'ctx t


