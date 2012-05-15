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

let html_of_writer writer = 
  let html = create () in
  let ()   = writer html in
  html 

let run js html = html.js := js :: !(html.js)
let str  s html = add_string html.html s

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

let implode list sep html = 
  let rec process = function 
    | [ ] -> ()
    | [x] -> x html
    | h :: t -> h html ; sep html ; process t
  in process list 

let get_html html = Buffer.contents html.html
let get_js   html = JsCode.seq (List.rev !(html.js)) 
  
let to_json writer =
  let html = html_of_writer writer in
  Json_type.Object [ "html", Json_type.String (get_html html) ;
		     "code", JsCode.to_json   (get_js   html) ]

let to_html_string writer = 
  let html = html_of_writer writer in
  get_html html 

module Convenience = struct

  let script src = 
    concat [ str "<script type=\"text/javascript\" src=\"" ;
	     esc src ;
	     str "\"></script>" ]

end

let print_page ?(css=[]) ?(js=[]) ?(head="") ?favicon ?(body_classes=[]) ~title writer more_js = 
  
  let html   = create () in
  let buffer = html.html in 

  (* HEAD elements *)
  add_string buffer "<!DOCTYPE html><html><head>" ;

  add_string buffer "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>" ;

  add_string buffer "<title>" ;
  esc_to_buffer title buffer ;
  add_string buffer "</title>" ;

  List.iter (fun css -> 
    add_string buffer "<link rel=\"stylesheet\" href=\"" ;
    esc_to_buffer css buffer ;
    add_string buffer "\"/>") css ;

  (match favicon with None -> () | Some link -> 
    add_string buffer "<link rel=\"shortcut icon\" href=\"" ;
    esc_to_buffer link buffer ;
    add_string buffer "\"/>") ;

  add_string buffer head ;

  (* BODY elements *)

  add_string buffer "</head><body" ;
  if body_classes <> [] then begin 
    add_string buffer " class=\"" ;
    esc_to_buffer (String.concat " " body_classes) buffer ;
    add_string buffer "\">" ;
  end else
    add_string buffer ">" ;

  writer html ;

  (* End-of-body SCRIPT elements *)

  concat (List.map Convenience.script js) html ;
  
  add_string buffer "<script type=\"text/javascript\">/*<![CDATA[*/" ;
  add_string buffer (JsCode.to_script (JsCode.seq [get_js html ; more_js])) ;
  add_string buffer "/*]]>*/</script>" ;  

  add_string buffer "</body></html>" ;

  (* Return the resulting string *)

  Buffer.contents buffer
