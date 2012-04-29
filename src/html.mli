(* Ohm is © 2012 Victor Nicollet *)

type js

type t = {
  html : Buffer.t ;
  js   : js 
}

val create : unit -> t

type writer = t -> unit

val add_js : JsCode.t -> writer
val esc    : string   -> writer
val str    : string   -> writer

val concat : writer list -> writer

val get_html : t -> string
val get_js   : t -> JsCode.t

val to_json   : t -> Json_type.t
val to_string : t -> string


module Convenience : sig
    
  val script : string -> writer

end

val print_page : 
     ?css:string list
  -> ?js:string list
  -> ?head:string
  -> ?body_classes:string list
  -> title:string
  -> t
  -> string
