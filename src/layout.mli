(* Jogging is © 2011 Victor Nicollet *)

module type CUSTOMIZABLE = sig

  module Html : 
  sig
    
    val script_src : string -> #View.Context.text View.t
    val script : JsCode.t -> View.Context.text View.t
    val link_stylesheet : string -> #View.Context.text View.t
      
  end

  type response 
    
  val render : 
       ?js_files     : string list
    -> ?css_files    : string list
    -> ?head         : View.Context.text View.t
    -> ?js           : JsCode.t
    -> ?body_classes : string list
    ->  title        : View.Context.text View.t
    ->  body         : View.Context.box  View.t
    ->  response -> response
    
end

module Customize : 
  functor (Action:Action.CUSTOMIZABLE) ->
    CUSTOMIZABLE with type response = Action.response
