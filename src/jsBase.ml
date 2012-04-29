(* Ohm is © 2011 Victor Nicollet *)

open JsCode
open Util
open BatPervasives

let to_js tree ctx = 
  View.implode (View.str ";") begin fun call ctx -> 
    ctx 
    |> View.str (call.name^"(")
    |> View.implode 
      (View.str ",") 
      (fun a -> View.str (Json_io.string_of_json ~recursive:true ~compact:true a))
      (call.args)
    |> View.str ")"
  end (list_of_tree tree) ctx

let to_event tree ctx = 
  View.implode (View.str ";") begin fun call ctx -> 
    ctx 
    |> View.str (call.name^".call(this")
    |> (if call.args = [] then (fun c -> c) else View.str ",")
    |> View.implode 
      (View.str ",") 
      (fun a -> View.esc (Json_io.string_of_json ~recursive:true ~compact:true a))
      (call.args)
    |> View.str ")"
  end (list_of_tree tree) ctx
  

let to_json tree = 
  Json_type.Build.list begin fun call ->
    Json_type.Build.array ( Json_type.Build.string call.name :: call.args )
  end (list_of_tree tree)
    
let staticInit = 
  make
    ~name:"jog.staticInit" 
    ~args:[]

let form id fields init = 
  make
    ~name:"jog.form" 
    ~args:[ Json_type.Build.string (Id.str id) ;
	    fields ;
	    init ]

let delay time action = 
  make
    ~name:"jog.delay"
    ~args:[ Json_type.Build.float time ;
	    to_json action ]

let init url = 
  make
    ~name:"jog.init"
    ~args:[ Json_type.Build.string url ]

let defineBox name id = 
  make 
    ~name:"jog.defineBox"
    ~args:[ Json_type.Build.string name ;
	    Json_type.Build.string (Id.str id) ]

let fillBox name render = 
  let html, js = View.extract render in 
  make
    ~name:"jog.fillBox"
    ~args:[ Json_type.Build.string name ;
	    Json_type.Build.string html ;
	    to_json (seq [staticInit ; js]) ]

let boxRefresh delay = 
  make ~name:"jog.boxRefresh" ~args:[ Json_type.Build.float delay ]

let boxInvalidate = 
  make ~name:"jog.boxInvalidate" ~args:[]

let boxLoad url = 
  make ~name:"jog.boxLoad" ~args:[ Json_type.Build.string url ]

let post url data = 
  make ~name:"jog.post" ~args:[ Json_type.Build.string url ; data ] 

type source = JsCode.call

let source ~name ~args =
  { name = name ; args = args }

let source_to_json source = 
  Json_type.Build.array (Json_type.Build.string source.name :: source.args)

type render = JsCode.call

let render ~name ~args =
  { name = name ; args = args }

let render_to_json render = 
  Json_type.Build.array (Json_type.Build.string render.name :: render.args)


