(* Ohm is © 2012 Victor Nicollet *)

open Util
open BatPervasives

open Action_Common

class type ['server,'args] request = object
  method self   : ('server,'args) Action_Endpoint.endpoint
  method server : 'server
  method path   : string
  method post   : [ `JSON of Json_type.t | `POST of (string,string) BatPMap.t ] option
  method get    : string -> string option 
  method args   : 'args
  method cookie : string -> string option          
end
  
class ['server,'args] nilreq (server:'server) (args:'args) = object

  method self (_:'server) (_:'args) = ""

  val server = server
  method server = server

  val args = args
  method args = args

  method path = ""
  method get (_:string) = (None : string option)
  method cookie (_:string) = (None : string option) 
  method post = (None : [ `JSON of Json_type.t | `POST of (string,string) BatPMap.t ] option)

end
 
class ['server,'args] fcgi_request 
  (endpoint:('server,'args) Action_Endpoint.endpoint)
  (server:'server) (args:'args) (cgi:Netcgi.cgi) =  

  let env = cgi # environment in 
  let post : [ `JSON of Json_type.t | `POST of (string,string) BatPMap.t ] option Lazy.t =
    lazy begin
      if cgi # request_method = `POST then
	if BatString.starts_with (env # input_content_type_string) "application/json" 
	then 
	  Some (`JSON 
		   (try
		      let field = (cgi # argument "BODY") # value in
		      match utf8 field with
			| Some field -> Json_io.json_of_string ~recursive:true field
			| None       -> Json_type.Null
		    with _ -> Json_type.Null))
	else 
	  Some (`POST 
		   (List.fold_left begin fun acc arg ->
		     try 
		       match utf8 (arg # name), utf8 (arg # value) with
			 | Some name, Some value -> BatPMap.add name value acc
			 | _ -> acc
		     with _ -> acc
		   end BatPMap.empty (cgi # arguments))) 
      else
	None
    end
  in
  let path = lazy (path_clean (env # cgi_script_name)) in

object 
  val path   = path
  val args   = args
  val server = server
  val post   = post
  val self   = endpoint

  method self   = self
  method args   = args
  method server = server
  method path   = Lazy.force path      
  method post   = Lazy.force post    

  method get field = 
    try 
      let field = ((cgi # argument field) # value) in
      utf8 field
    with Not_found -> None 
  		
  method cookie name = 
    try 
      let cookie = env # cookie name in
      let value  = Netcgi.Cookie.value cookie in
      utf8 value
    with Not_found -> None
      
end
