(* Ohm is © 2011 Victor Nicollet *)

open Util
open BatPervasives

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

module Customize = 
  functor (Action:Action.CUSTOMIZABLE) ->
struct

  type response = Action.response

  module Html = 
  struct
    
    let script_src src ctx =     
      ctx
      |> View.str "<script type=\"text/javascript\" src=\"" 
      |> View.esc src 
      |> View.str "\"></script>"
	  
    let script source ctx =
      ctx
      |> View.str "<script type=\"text/javascript\">/*<![CDATA[*/"
      |> JsBase.to_js source
      |> View.str "/*]]>*/</script>"  
	  
    let script_write view ctx =
      ctx
      |> View.str "<script type=\"text/javascript\">/*<![CDATA[*/"
      |> view
      |> View.str "/*]]>*/</script>"  
	  
    let link_stylesheet href ctx =
      ctx
      |> View.str "<link rel=\"stylesheet\" href=\""
      |> View.esc href
      |> View.str "\"/>"
	  
  end
    
  let render 
      ?(js_files=[])
      ?(css_files=[])
      ?(head=View.concat [])
      ?(js=JsCode.seq [])
      ?(body_classes=[])
      ~title ~body response =
 
    let body, body_js = View.extract body in 
    response 
    |> Action.html begin fun js ctx -> ctx
	|> View.str "<!doctype html><html><head>"
	|> View.str "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">"
	|> View.str "<title>"
	|> title
	|> View.str "</title>"
	|> head
	|> View.foreach Html.link_stylesheet css_files
	|> View.str ""
	|> begin
	  if body_classes <> [] then 
	    View.str "</head><body class=\"" 
	    |- View.esc (String.concat " " body_classes)
	    |- View.str "\">"
	  else
	    View.str "</head><body>"
	end 
	|> View.str body
	|> View.foreach Html.script_src js_files
	|> Html.script_write js 
	|> View.str "</body></html>"
    end
    |> Action.javascript (JsCode.seq [body_js;js])
	
end
