(* Ohm is © 2012 Victor Nicollet *)

open Util
open BatPervasives

open Action_Common

include Action_Server
include Action_Response 
include Action_Request
include Action_Endpoint
  
type ('server,'args) controller = 'server server * string * 'args Args.t

type ('server,'args) t = ('server,'args) request -> response -> (unit,response) Run.t

let declared = ref []
let defined  = Hashtbl.create 100     

let dispatch_define (server,prefix,args) action = 
  if Util.role = `Web then begin 

    let key   = path_clean (lowercase prefix) in 

    let value protocol domain port suffix cgi =
      match Args.parse args suffix with None -> None | Some args ->
	match server # matches protocol domain port with None -> None | Some s ->
	  Some (server # cookie_domain, action (new fcgi_request s args cgi)) 
    in

    Hashtbl.add defined key value
	
  end

let dispatch_declare (server,prefix,args) = 
  let cell = ref (Some (path_clean (lowercase prefix))) in
  declared := cell :: !declared ;
  cell
    
let ensure () = 
  if !declared <> [] then begin 
    List.iter (fun cell -> 
      match !cell with 
	| None -> () 
	| Some key -> Util.log "Action: FAIL : action %S declared but not defined" key
    ) !declared ;
    declared := [] ;
  end

let declare server prefix args = 
  let controller = server, prefix, args in
  let cell = dispatch_declare controller and endpoint = endpoint_of_controller controller in 
  endpoint, fun action -> 
    (match !cell with None -> () 
      | Some key -> Util.log "Action: FAIL : action %S defined twice" key) ;
    cell := None ;
    dispatch_define controller action 
    
let register server prefix args action = 
  let controller = server, prefix, args in
  dispatch_define controller action ;
  endpoint_of_controller controller 
  
let find_strict protocol domain port prefix suffix cgi = 
  let list = Hashtbl.find_all defined (lowercase (path_clean prefix)) in 
  try Some (BatList.find_map (fun candidate -> candidate protocol domain port suffix cgi) list)
  with Not_found -> None

let slice prefix suffix = 
  try let path, cut = BatString.rsplit prefix "/" in 
      Some (path, cut :: suffix) 
  with Not_found -> 
    if prefix = "" then None else Some ("", prefix :: suffix) 

let find protocol domain port path cgi =
  ensure () ;
  let rec aux prefix suffix = 
    match find_strict protocol domain port prefix suffix cgi with Some a -> Some a | None ->
      match slice prefix suffix with None -> None | Some (prefix, suffix) ->
	aux prefix suffix
  in aux path []
      
exception Action_not_found of string
    
let dispatch cgi = 
  
  let cgi = (cgi :> Netcgi.cgi) in
  
  let env = cgi # environment in 
  
  let domain   = env # cgi_server_name in
  let defport, protocol = if env # cgi_https then 443,`HTTPS else 80,`HTTP in
  let port     = BatOption.default defport (env # cgi_server_port) in
  let path     = path_clean (env # cgi_script_name) in
  
  let cookie_suffix, action = match find protocol domain port path cgi with 
    | Some (cookie_suffix,action) -> cookie_suffix, action
    | None -> raise (Action_not_found ("http://"^(env # cgi_server_name)^"/"^path))
  in
  
  let response = Run.eval () (action empty) in
  
  process cookie_suffix cgi response 
    
let run callback = 
  Netcgi_fcgi.run 
    ~config:{
      Netcgi.default_config with Netcgi.permitted_input_content_types = 
	[ "application/json" ; "multipart/form-data" ; "application/x-www-form-urlencoded" ] 
    }
    callback
    
module Convenience = struct

  let nilreq s a = new nilreq s a

  let single_domain_server ?(secure=false) ?port ?cookies domain = 
    let defport, protocol = if secure then 443,`HTTPS else 80,`HTTP in
    let port = BatOption.default defport port in
    (object
      method protocol  = protocol
      method domain () = domain
      method port   () = port
      method cookie_domain = cookies
      method matches pr dom po = 	if po = port && pr = protocol && domain = dom then Some () else None
     end)

  let sub_domain_server ?(secure=false) ?port ?cookies suffix = 
    let defport, protocol = if secure then 443,`HTTPS else 80,`HTTP in
    let port = BatOption.default defport port in
    let cut  = String.length suffix in
    (object
      method protocol = protocol
      method domain s = s ^ suffix
      method port   _ = port
      method cookie_domain = cookies
      method matches pr dom po = 
	if po = port && pr = protocol && BatString.ends_with dom suffix
	then Some (BatString.left dom (String.length dom - cut)) else None
     end)
    
  let root s = server_root s
      
end
