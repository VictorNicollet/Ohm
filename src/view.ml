(* Ohm is © 2011 Victor Nicollet *)

open Util

type channel = string -> int -> int -> unit
type 'ctx t = 'ctx -> 'ctx

module Context = 
struct

  class type text = 
  object ('ctx)
    method write   : channel
  end

  class type box = 
  object ('ctx)
    method write       : channel
    method add_js_code : JsCode.t -> 'ctx
    method get_js_code : JsCode.t
  end

  let add_js_code code ctx = 
    ctx # add_js_code code

end

type html = Context.box  t
type text = Context.text t

class channel_writer (chan : Netchannels.rec_out_channel) = 
object (self)
  method write string beg len = ignore (chan # output string beg len) 
end

let _esc_html chan = 
  fun string beg len ->
    let b = ref beg in
      for m = beg to beg + len - 1 do
	match string.[m] with
	  | '<' -> chan string !b (m - !b) ; chan "&lt;" 0 4 ;    b := m+1 
	  | '>' -> chan string !b (m - !b) ; chan "&gt;" 0 4 ;    b := m+1 
	  | '&' -> chan string !b (m - !b) ; chan "&amp;" 0 5 ;   b := m+1 
	  | '"' -> chan string !b (m - !b) ; chan "&quot;" 0 6 ;  b := m+1 
	  | _ -> ()
      done ;
      if !b < beg + len then
	chan string !b (beg + len - !b)

let str str ctx = 
  ctx # write str 0 (String.length str) ; ctx

let esc str ctx = 
  _esc_html (ctx # write) str 0 (String.length str) ; ctx 

let int int ctx = 
  str (string_of_int int) ctx

let write_to_string writer = 
  let context = object 
    val buf = Buffer.create 100
    method write string beg len = Buffer.add_substring buf string beg len 
    method result = Buffer.contents buf
  end in 
    ignore (writer (context :> Context.text)) ; 
    context # result

let concat list ctx =
  List.fold_left (fun ctx write -> write ctx) ctx list

let foreach map list ctx = 
  List.fold_left (fun ctx x -> map x ctx) ctx list

let implode (sep : 'a t) (map : 'b -> 'a t) (list : 'b list) (ctx : 'a) = 
  let rec aux ctx = function 
    | [] -> ctx
    | [x] -> map x ctx
    | h::t -> aux (sep (map h ctx)) t
  in aux ctx list

let extract writer = 
  let html = Buffer.create 100 in
  let ctx = object 
    val code = []
    method write string beg len = Buffer.add_substring html string beg len
    method add_js_code t = {< code = t :: code >}
    method get_js_code   = JsCode.seq (List.rev code)
  end in 

  let ctx = writer (ctx :> Context.box) in
  
  Buffer.contents html , ctx # get_js_code
