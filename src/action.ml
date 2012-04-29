(* Ohm is © 2011 Victor Nicollet *)

open Util
open BatPervasives
module BS = BatString

module type CONFIG = sig
  type server
  val name_of_server : server -> string
  val server_of_name : string -> server
  val server_suffix  : server -> string option
end

(* ----------------------------------------------------------------- *)

module SingleServer = struct
  type server = [ `TheServer ]
  let name_of_server _ = "http://.../"
  let server_of_name _ = `TheServer
  let server_suffix  _ = None
end

(* ----------------------------------------------------------------- *)

let path_clean path = 
  if BS.is_empty path then path 
  else let path = 
	 if BS.ends_with path "/" then BS.rchop path 
	 else path 
       in if BS.starts_with path "/" then BS.lchop path
	 else path

(* ----------------------------------------------------------------- *)

module type CUSTOMIZABLE = sig

  type response 

  val html : (View.Context.text View.t -> View.Context.text View.t) -> response -> response
  val redirect : string -> response -> response
  val json : (string * Json_type.t) list -> response -> response
  val with_cookie : name:string -> value:string -> life:int -> response -> response
  val file : file:string -> mime:string -> data:View.Context.text View.t -> response -> response
  val javascript : JsCode.t -> response -> response
  val more_javascript : JsCode.t -> response -> response

  class type request =
  object
    method path : string
    method servername : string
    method format : [ `JSON | `URLENCODED ]
    method post : string -> string option
    method postlist : (string * string) list
    method json : Json_type.t 
    method args : int -> string option
    method cookie : string -> string option
    method query : string 
    method ip : string

  end

  type t = request -> response -> response

  type server 

  class controller : server -> string ->
  object
    method server : server
    method path   : string
  end

  exception Action_not_found of string

  val action_of_path : server -> string -> 
    ((request -> response -> response) * string list) option
  val dispatch : #Netcgi.cgi -> unit
  val register : #controller -> t -> unit
  val run : (Netcgi_fcgi.cgi -> unit) -> unit    
end

module Customize = functor (Config:CONFIG) -> struct

  type response_kind = 
    | Html of (View.Context.text View.t -> View.Context.text View.t) * JsCode.t
    | Redirect of string
    | Json of (string * Json_type.t) list * JsCode.t
    | File of string * string * View.Context.text View.t
	
  type response = 
      {
	kind : response_kind ;
	cookies : (string * string * int) list
      }
	
  let redirect url response = {
    response with 
      kind = Redirect url
  }
    
  let more_javascript new_js response = { 
    response with 
      kind = begin match response.kind with 
	| Html (f,js) -> Html (f, JsCode.seq [js;new_js])
	| Json (j,js) -> Json (j, JsCode.seq [js;new_js])
	| keep        -> keep 
      end
  }
    
  let javascript new_js response = { 
    response with 
      kind = begin match response.kind with 
	| Html (f,js) -> Html (f, JsCode.seq [js;new_js])
	| Json (j,js) -> Json (j, JsCode.seq [js;new_js])
	| _           -> Json ([], new_js)
      end
  }
    
  let with_cookie ~name ~value ~life response = {
    response with 
      cookies = (name, value, life) :: response.cookies
  }
    
  let file ~file ~mime ~data response = {
    response with 
      kind = File (file, mime, data)
  }
    
  let json json response = {
    response with 
      kind = begin match response.kind with
	| Html (_,js) -> Json (json, js)
	| Json (f,js) -> Json (json @ f, js)
	| _           -> Json (json, JsCode.seq [])
      end
  }
    
  let html view response = {
    response with
      kind = begin match response.kind with
	| Html (_,js) 
	| Json (_,js) -> Html (view, js)
	| _           -> Html (view, JsCode.seq [])
      end
  }

(* ----------------------------------------------------------------- *)

  class type request =
  object 
    method path : string
    method format : [`JSON|`URLENCODED]
    method json : Json_type.t
    method servername : string
    method post : string -> string option
    method postlist : (string * string) list
    method args : int -> string option
    method cookie : string -> string option
    method query : string 
    method ip : string
  end
    
  class fcgi_request (args : string list) (cgi : Netcgi.cgi) =
    let env = cgi # environment in 
    let format =
      if BatString.starts_with (env # input_content_type_string) "application/json" 
      then `JSON
      else `URLENCODED
    in
    let path = env # cgi_script_name in
    let json = 
      lazy (
	try
	  let field  = (cgi # argument "BODY") # value in
	  match utf8 field with
	    | Some field -> Json_io.json_of_string ~recursive:true field
	    | None       -> Json_type.Null
	with _ -> Json_type.Null
      )
    in
  object 
    val path = path_clean path
    val servername = env # cgi_server_name
      
    method args n = 
      try 
	let arg = BatList.at args n in
	utf8 arg
      with Invalid_argument _ -> None
	
    method path = path      
      
    method servername = servername
      
    method format = ( format : [`JSON|`URLENCODED] )

    method json = 
      if format = `JSON then
	Lazy.force json 
      else
	Json_type.Null

    method post field = 
      if format = `URLENCODED then 
	try 
	  let field = ((cgi # argument field ) # value) in
	  utf8 field
	with Not_found -> None 
      else 
	None
	
    method ip = env # cgi_remote_addr
      
    method query = 
      env # cgi_property ~default:"" "QUERY_STRING"
	
    method postlist = 
      if format = `URLENCODED then
	BatList.filter_map begin fun arg ->
	  try 
	    match utf8 (arg # name), utf8 (arg # value) with
	      | Some name, Some value -> Some (name,value) 
	      | _ -> None
	  with _ -> None
	end (cgi # arguments) 
      else 
	[]
	
    method cookie name = 
      try 
	let cookie = env # cookie name in
	let value  = Netcgi.Cookie.value cookie in
	utf8 value
      with Not_found -> None
	
  end

  let empty = { kind = Json ( [] , JsCode.seq [] ) ; cookies = [] }

  type server = Config.server

  class controller server path = 
  object
    val server = (server : server)
    val path   = (path : string)
      
    method server = server
    method path   = path
  end
    
  type t = request -> response -> response
    
  let _path_hash = Hashtbl.create 100
    
  let _servername = Config.name_of_server 
    
  let register ctrl action = 
    if Util.role = `Web then begin 
      let path   = lowercase (ctrl # path) in
      log  "Action.register: http://%s/%s" (_servername (ctrl # server)) path ;
      Hashtbl.add _path_hash (ctrl # server,path) action
    end 

(* ----------------------------------------------------------------- *)

  let (>>) x f = f x

  let add_code js json = 
    try let code = List.assoc "code" json in 
	let js'  = match code with Json_type.Array l -> l | _ -> [] in
	let js'' = match js   with Json_type.Array l -> l | _ -> [] in
	("code", Json_type.Array (js' @ js'')) :: json
    with Not_found -> ("code", js) :: json

  let _process server (cgi : Netcgi.cgi) response = 

    let cookies = 
      List.map (fun (name,value,age) -> 
	let age = if age = 0 then None else Some age in
	Netcgi.Cookie.make ?max_age:age ?domain:(Config.server_suffix server) ~path:"/" name value
      ) response.cookies
    in
    
    let out_channel = (cgi # environment # out_channel :> Netchannels.rec_out_channel) in

    begin match response.kind with 
  
    | Html (write,js) ->
      if cookies <> [] then cgi # set_header ~set_cookies:cookies () ;            
      cgi # environment # send_output_header () ;
      ignore (write (JsBase.to_js js) (new View.channel_writer out_channel))
	
    | Redirect url ->
      cgi # set_redirection_header ~set_cookies:cookies url ;
      cgi # environment # send_output_header () 

    | File (file, mime, data) ->
      cgi # set_header ~set_cookies:cookies ~content_type:mime ~filename:file ();
      cgi # environment # send_output_header () ;
      ignore (data (new View.channel_writer out_channel))
      	
    | Json (json,js) ->
      cgi # set_header ~set_cookies:cookies ~content_type:"application/json" ();
      cgi # environment # send_output_header () ;
      let full = add_code (JsBase.to_json js) json in  
      let json = 
	List.fold_left (fun acc (name,value) ->
	  try ignore (List.assoc name acc) ; acc with Not_found -> (name,value) :: acc
	) [] full
	>> Json_type.Build.objekt
	>> (Json_io.string_of_json ~recursive:true ~compact:true)
      in
      ignore (View.str json (new View.channel_writer out_channel))
    end 
	
  let action_of_path server path = 
    let path = path_clean path in 
    let rec findstar path removed = 
      let attempt = if BS.is_empty path then "*" else path ^ "/*" in    
      try Some (Hashtbl.find _path_hash (server, lowercase attempt), removed) with Not_found ->
	if BS.is_empty path then None else 
	  try let (path,cut) = BS.rsplit path "/" in findstar path (cut::removed)
	  with Not_found -> findstar "" (path::removed)
    in try Some (Hashtbl.find _path_hash (server,lowercase path), []) with Not_found -> findstar path []
      
  exception Action_not_found of string
            
  let dispatch cgi = 
    
    let cgi = (cgi :> Netcgi.cgi) in
    
    let env = cgi # environment in 
    
    let server = Config.server_of_name (env # cgi_server_name) in
    
    let path = path_clean (env # cgi_script_name) in
    
    let action, args   = match action_of_path server path with 
      | Some found -> found
      | None -> raise (Action_not_found ("http://"^(_servername server)^"/"^(path)))
    in
    
    let request = (new fcgi_request args cgi :> request) in
    
    let response = action request empty in

    _process server cgi response 

  let run callback = 
    Netcgi_fcgi.run 
      ~config:{
	Netcgi.default_config with Netcgi.permitted_input_content_types = 
	  [ "application/json" ; "multipart/form-data" ; "application/x-www-form-urlencoded" ] 
      }
      callback
      
end
