(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives
open Buffer

type js = JsCode.t list ref

type t = {
  html : Buffer.t ;
  js   : js
}

let create () = { html = Buffer.create 16 ; js = ref [] }

type writer = t -> unit

let add_js js html = html.js := js :: !(html.js)
let str     s html = add_string html.html s

let esc_to_buffer s buf = 
  let b    = ref 0 in
  let len  = String.length s in
  for m = 0 to len - 1 do
    match s.[m] with
      | '<' -> let () = add_substring buf s !b (m - !b) in
	       let () = add_string    buf "&lt;" in
	       b := m+1 
      | '>' -> let () = add_substring buf s !b (m - !b) in
	       let () = add_string    buf "&gt;" in
	       b := m+1 
      | '&' -> let () = add_substring buf s !b (m - !b) in
	       let () = add_string    buf "&amp;" in
	       b := m+1 
      | '"' -> let () = add_substring buf s !b (m - !b) in
	       let () = add_string    buf "&quot;" in
	       b := m+1 
      | _ -> ()
  done ;
  if !b < len then
    add_substring buf s !b (len - !b)

let esc s html = 
  esc_to_buffer s html.html

let concat list html = List.iter ((|>) html) list

let get_html html = Buffer.contents html.html
let get_js   html = JsCode.seq (List.rev !(html.js)) 

let to_string html = 
  get_html html 
  ^ "<script type=\"text/javascript\"><![CDATA[" 
  ^ JsCode.to_script (get_js html)
  ^ "]]></script>"

let to_json html = 
  Json_type.Object [ "html", Json_type.String (get_html html) ;
		     "code", JsCode.to_json   (get_js   html) ]

module Convenience = struct

  let script src = 
    concat [ str "<script type=\"text/javascript\" src=\"" ;
	     esc src ;
	     str "\"></script>" ]

end

let print_page ?(css=[]) ?(js=[]) ?(head="") ?(body_classes=[]) ~title html = 
  
  concat (List.map Convenience.script js) html ;

  let buffer = Buffer.create 2000 in

  add_string buffer "<!DOCTYPE html><html><head>" ;

  add_string buffer "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>" ;

  add_string buffer "<title>" ;
  esc_to_buffer title buffer ;
  add_string buffer "</title>" ;

  List.iter (fun css -> 
    add_string buffer "<link rel=\"stylesheet\" href=\"" ;
    esc_to_buffer css buffer ;
    add_string buffer "\"/>") css ;

  add_string buffer head ;

  add_string buffer "</head><body" ;
  if body_classes <> [] then begin 
    add_string buffer " class=\"" ;
    esc_to_buffer (String.concat " " body_classes) buffer ;
    add_string buffer "\">" ;
  end else
    add_string buffer ">" ;

  add_string buffer (to_string html) ;

  add_string buffer "</body></html>" ;

  Buffer.contents buffer
