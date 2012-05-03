(* Ohm is © 2012 Victor Nicollet *)

open Util
open BatPervasives

type response_kind = 
  | Page of (JsCode.t -> string) * JsCode.t
  | Redirect of string
  | Json of (string * Json_type.t) list * JsCode.t
  | File of string * string * string
      
type response = 
    {
      kind : response_kind ;
      cookies : (string * string * int) list
    }

let empty = { kind = Json ( [] , JsCode.seq [] ) ; cookies = [] }
      
let redirect url response = {
  response with 
    kind = Redirect url
}
  
let more_javascript new_js response = { 
  response with 
    kind = begin match response.kind with 
      | Json (j,js) -> Json (j, JsCode.seq [js;new_js])
      | Page (p,js) -> Page (p, JsCode.seq [js;new_js])
      | keep        -> keep 
    end
}
  
let javascript new_js response = { 
  response with 
    kind = begin match response.kind with 
      | Json (j,js) -> Json (j, JsCode.seq [js;new_js])
      | Page (p,js) -> Page (p, JsCode.seq [js;new_js])
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
      | Page (_,js) -> Json (json, js)
      | Json (f,js) -> Json (json @ f, js)
      | _           -> Json (json, JsCode.seq [])
    end
}
  
let page html response = {
  response with
    kind = begin match response.kind with
      | Page (_,js) 
      | Json (_,js) -> Page (html, js)
      | _           -> Page (html, JsCode.seq [])
    end
}

let add_code js json = 
  try let code = List.assoc "code" json in 
      let js'  = match code with Json_type.Array l -> l | _ -> [] in
      let js'' = match js   with Json_type.Array l -> l | _ -> [] in
      ("code", Json_type.Array (js' @ js'')) :: json
  with Not_found -> ("code", js) :: json
    
let process suffix (cgi : Netcgi.cgi) response = 
  
  let cookies = 
    List.map (fun (name,value,age) -> 
      let age = if age = 0 then None else Some age in
      Netcgi.Cookie.make ?max_age:age ?domain:suffix ~path:"/" name value
    ) response.cookies
  in
  
  let out_channel = (cgi # environment # out_channel :> Netchannels.rec_out_channel) in
  
  begin match response.kind with 
  	
    | Page (html,js) ->
      let html = html js in 
      if cookies <> [] then cgi # set_header ~set_cookies:cookies () ;            
      cgi # environment # send_output_header () ;
      ignore (out_channel # output html 0 (String.length html)) 

    | Redirect url ->
      cgi # set_redirection_header ~set_cookies:cookies url ;
      cgi # environment # send_output_header () 

    | File (file, mime, data) ->
      cgi # set_header ~set_cookies:cookies ~content_type:mime ~filename:file ();
      cgi # environment # send_output_header () ;
      ignore (out_channel # output data 0 (String.length data)) 
      	
    | Json (json,js) ->
      cgi # set_header ~set_cookies:cookies ~content_type:"application/json" ();
      cgi # environment # send_output_header () ;
      let full = add_code (JsCode.to_json js) json in  
      let json = 
	List.fold_left (fun acc (name,value) ->
	  try ignore (List.assoc name acc) ; acc with Not_found -> (name,value) :: acc
	) [] full
        |> Json_type.Build.objekt
	|> (Json_io.string_of_json ~recursive:true ~compact:true)
      in
      ignore (out_channel # output json 0 (String.length json)) 
  end 
    
