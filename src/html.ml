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

let esc     s html = 
  let b    = ref 0 in
  let len  = String.length s in
  let html = html.html in
  for m = 0 to len - 1 do
    match s.[m] with
      | '<' -> let () = add_substring html s !b (m - !b) in
	       let () = add_string    html "&lt;" in
	       b := m+1 
      | '>' -> let () = add_substring html s !b (m - !b) in
	       let () = add_string    html "&gt;" in
	       b := m+1 
      | '&' -> let () = add_substring html s !b (m - !b) in
	       let () = add_string    html "&amp;" in
	       b := m+1 
      | '"' -> let () = add_substring html s !b (m - !b) in
	       let () = add_string    html "&quot;" in
	       b := m+1 
      | _ -> ()
  done ;
  if !b < len then
    add_substring html s !b (len - !b)


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
