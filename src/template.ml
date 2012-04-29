(* Ohm is © 2011 Victor Nicollet *)

open Util
open BatPervasives

module Parse = struct

  type token = 
    | Javascript of string * string list
    | Var        of string
    | Str        of string
    | I18n       of string

  let _parse loader view =

    let parse_javascript s =
      let s = String.sub s 3 (String.length s - 4) in
      match BatString.nsplit s ":" with
	| []           -> []
	| name :: args -> [ Javascript (name,args) ]
    in

    let read_load s = 
      Str.full_split (Str.regexp "{i:[a-z][-a-zA-Z0-9_.]*}") s 
      |> List.map (function 
	  | Str.Delim s -> let key = String.sub s 3 (String.length s - 4) in 
			   loader key
	  | Str.Text s  -> [Str s])
      |> List.concat
    in

    let read_javascript s =
      Str.full_split (Str.regexp "{j\\(:[a-z][-a-zA-Z0-9_.]*\\)+}") s 
      |> List.map (function 
	  | Str.Delim s -> parse_javascript s 
	  | Str.Text  s -> read_load s)
      |> List.concat
    in

    let read_i18n s = 
      Str.full_split (Str.regexp "{t:[a-z][-a-zA-Z0-9_.]*}") s 
      |> List.map (function
	  | Str.Delim s -> [ I18n (String.sub s 3 (String.length s - 4)) ]
	  | Str.Text  s -> read_javascript s)
      |> List.concat
    in

    let read_vars s = 
      Str.full_split (Str.regexp "{v:[a-z][-a-zA-Z0-9_.]*}") s
      |> List.map (function 
	  | Str.Delim s -> [ Var (String.sub s 3 (String.length s - 4)) ]
	  | Str.Text  s -> read_i18n s) 
      |> List.concat
    in

    read_vars view

  let rec _cleanup = function
    | (Str "") :: t -> _cleanup t
    | (Str a) :: (Str b) :: t -> _cleanup ( Str (a^b) :: t ) 
    | h :: t -> h :: _cleanup t
    | []     -> []
      
  let parse loader view = 
    _parse loader view |> _cleanup

end    

type ('arg,'ctx) template = 'arg -> I18n.t -> 'ctx View.t 

type ('arg,'ctx) t = ('arg,'ctx) template

module type JS_HTML_TEMPLATE_DEF = sig
  type t 
  val source : I18n.language -> string
  val script : I18n.language -> (string * (t -> Json_type.t)) list
  val mapping : I18n.language -> (string * ((t, View.Context.box) template -> (t, View.Context.box) template)) list
end

module type HTML_TEMPLATE_DEF = sig
  type t 
  val source : I18n.language -> string
  val mapping : I18n.language -> (string * ((t, View.Context.box) template -> (t, View.Context.box) template)) list
end

module type TEXT_TEMPLATE_DEF = sig
  type t 
  val source : I18n.language -> string
  val mapping : I18n.language -> (string * ((t, View.Context.text) template -> (t, View.Context.text) template)) list
end

module type HTML = sig
  type t 
  val template : I18n.language -> (t, View.Context.box) template
  val render : t -> I18n.t -> View.Context.box -> View.Context.box
end

module type TEXT = sig
  type t 
  val template : I18n.language -> (t, View.Context.text) template
  val render : t -> I18n.t -> View.Context.text -> View.Context.text
end

module Mk = struct

  let _json str = 
    Json_io.string_of_json ~recursive:true (Json_type.Build.string str) 
      
  let empty _ _ c = c
  let put str  t a i c = View.str str (t a i c) 
  let esc f    t a i c = View.esc (f a) (t a i c)
  let str f    t a i c = View.str (f a) (t a i c) 
  let js code  t a i c = View.Context.add_js_code code (t a i c)
  let code f   t a i c = View.Context.add_js_code (f a) (t a i c)
  let html w    t a i c = w a (t a i c)  
  let text w   t a i c =
    let c = (t a i c) in
    let _ = w a (c :> View.Context.text) in
    c 

  let verbatim what _ c = what c

  let int  f   t a i c = View.str (string_of_int (f a)) (t a i c)

  let i18n lbl t a i c = I18n.get i lbl (t a i c) 
  let trad f   t a i c = I18n.get i (f a) (t a i c)

  let iput str  t a i c = View.str (str i) (t a i c) 
  let iesc f    t a i c = View.esc (f a i) (t a i c)
  let istr f    t a i c = View.str (f a i) (t a i c) 
  let ijs code  t a i c = View.Context.add_js_code (code i) (t a i c)
  let icode f   t a i c = View.Context.add_js_code (f a i) (t a i c)      
  let ihtml w   t a i c = w a i (t a i c)
  let itext w   t a i c =
    let c = (t a i c) in
    let _ = w a i (c :> View.Context.text) in
    c 
      
  let sub f v  t a i c = v (f a) i (t a i c)

  let sub_or f v d t a i c =
    match f a with 
      | None -> d a i (t a i c) 
      | Some x -> v x i (t a i c)
      
  let list f v t a i c =
    List.fold_left (fun c x -> v x i c) (t a i c) (f a)

  let list_or f v d t a i c =
    let list = f a in 
    if list = [] then 
      d a i (t a i c)
    else
      List.fold_left (fun c x -> v x i c) (t a i c) list
      
end

let to_html t a i c = t a i c
let to_text t a i c = t a i c

module Compile = struct

  type ('arg,'ctx) init = {
    name   : string ;
    tokens : Parse.token list ;
    map    : (string * (('arg,'ctx) t -> ('arg,'ctx) t)) list ;
    jsargs : (string * ('arg -> Json_type.t)) list 
  }

  let rawcompile fold init = List.fold_left (fold init) Mk.empty init.tokens 

  let fold init t = function
    | Parse.Var n -> 
      begin
	try (List.assoc n init.map) t
	with Not_found -> 
	  log "Template.compile: undefined variable {v:%s} in view %s" n init.name ;  
	  Mk.put ("{v:"^n^"}") t
      end
    | Parse.I18n n -> Mk.i18n (`label n) t
    | Parse.Str  a -> Mk.put  a t
    | Parse.Javascript (n,a) -> 
	  log "Template.compile: javascript {j:%s} in HTML view %s" n init.name ;  
	  Mk.put ("{j:"^n^"}") t
            
  let compile init = rawcompile fold init

  let foldjs init t = function
    | Parse.Javascript (n,a) -> 
      begin
	let args = List.map (fun arg ->
	  try List.assoc arg init.jsargs
	  with Not_found -> 
	    log "Template.compile: undefined js parameter {j:%s} in view %s" arg init.name ;
	    fun _ -> Json_type.Null
	) a in
	Mk.code (fun data -> JsCode.make n (List.map (fun f -> f data) args)) t
      end
    | other -> fold init t other
    
  let compilejs init = rawcompile foldjs init
      
end

module type LOADER = sig 

  val load : string 
    -> string
    -> (string * (('arg,(#View.Context.text as 'ctx)) t -> ('arg,'ctx) t)) list 
    -> [< `Text | `Html]
    -> ('arg,'ctx) t 

  val jsload : string 
    -> string
    -> (string * (('arg,View.Context.box) t -> ('arg,View.Context.box) t)) list 
    -> (string * ('arg -> Json_type.t)) list
    -> [< `Text | `Html]
    -> ('arg,View.Context.box) t 

  val clear : unit -> unit

  val save : unit -> unit

  module MakeLoader : 
    functor (From : sig val from : string end) ->
  sig

    module Html   : functor (Def:   HTML_TEMPLATE_DEF) -> HTML with type t = Def.t
    module JsHtml : functor (Def:JS_HTML_TEMPLATE_DEF) -> HTML with type t = Def.t
    module Text   : functor (Def:   TEXT_TEMPLATE_DEF) -> TEXT with type t = Def.t

  end

end

module Loader = 
  functor (DB:CouchDB.DATABASE) ->
struct

  let id = Id.of_string "templates"

  module Template = Fmt.Make(struct
    type json t = (string * string) assoc 
  end)

  module MyTable = CouchDB.Table(DB)(Id)(Template)

  let cache = Hashtbl.create 1000 

  (* Keeping track of all loaded templates to estimate size *)
  let loaded = ref ([] : unit list) 
  let add_loaded x = loaded := (Obj.magic x) :: !loaded ; x
    
  (* Loads the list of all views from the database (if in a mode that is expected to do this)
     and places it in a cache from where the loaders will read. Do not clear that cache until
     the loaders are done. *)
  let load =
    if Util.role <> `Put then 
      let list = BatOption.default [] (MyTable.get id |> Run.eval (new CouchDB.init_ctx)) in
      List.iter (fun (key,value) -> Hashtbl.add cache key value) list
	
  (* Clears the cache *)
  let clear () = 
    Hashtbl.clear cache 

  (* Writes the data back to the database in Put mode. Call before the cache is cleared. *)
  let save () = 
 
    if Util.role = `Put then (

      Util.log "Ohm.Template : estimated %d bytes of memory used"
	(String.length (Marshal.to_string (!loaded) [Marshal.Closures])) ;

      let doc = Hashtbl.fold (fun key value acc -> (key,value)::acc) cache [] in
      MyTable.transaction id (MyTable.insert doc) |> Run.eval (new CouchDB.init_ctx) |> ignore

    ) 

  (* Two whitespace compression functions, which depend on whether text or HTML
     is being loaded *)
  let compress = function 
    | `Text -> identity 
    | `Html -> (fun content -> 
      content
	|> Str.global_replace (Str.regexp "[\t\n\r]") " "
	|> Str.global_replace (Str.regexp " +") " " 
	|> Str.global_replace (Str.regexp "> *<") "><"
    )

  (* Parses and inserts one or more views into the cache. *)
  let insert compress prefix root contents = 
    let regex  = Str.regexp "{{[-a-z]+:\\|}}" in
    let tokens = Str.full_split regex contents in 
    let put prefix name contents = Hashtbl.add cache (prefix^"/"^name) (compress contents) in
    let rec parse prefix name accum = function
      | [] -> put prefix name (Buffer.contents accum) ; []
      | Str.Text t :: list -> 
	Buffer.add_string accum t ;
	parse prefix name accum list			     
      | Str.Delim "}}" :: list -> 
	put prefix name (Buffer.contents accum) ; 
	list
      | Str.Delim str :: list -> 
	let suffix = String.sub str 2 (String.length str - 3) in
	Buffer.add_string accum ("{v:" ^ suffix ^ "}") ;
	parse prefix name accum (
	  parse prefix (name ^ "/" ^ suffix) (Buffer.create 80) list
	)
    in
    
    match tokens with 
      | [ Str.Text t ] -> put prefix root contents 
      | list -> ignore (parse prefix root (Buffer.create 80) list)
			     
  (* Reads a view from the cache. Optionally loads it from a file when necessary. *)
  let read prefix name kind =     
    try Some (Hashtbl.find cache (prefix^"/"^name)) with Not_found -> 
      if Util.role = `Put then       
	let root_name = try fst (BatString.split name "/") with _ -> name in
	let file = prefix ^ "/" ^ root_name ^ ".htm" in
	match Util.get_view_contents file with None -> None | Some contents ->
	  ( insert (compress kind) prefix root_name contents ; 
	    try Some (Hashtbl.find cache (prefix^"/"^name)) with Not_found -> None )
      else
	None

  let rec load_tokens kind prefix name =
    match read prefix name kind with 
      | Some data -> Parse.parse (load_tokens kind "helpers") data
      | None -> let () = log "View.load: FAIL %s : not found" (prefix^"/"^name) in
		[ Parse.Str ("View `"^prefix^"/"^name^"` not found") ]
    
  let load prefix name map kind = 
    let tokens = load_tokens kind prefix name in 
    add_loaded (Compile.compile Compile.({ name ; tokens ; map ; jsargs = [] }))
    
  let jsload prefix name map jsargs kind = 
    let tokens = load_tokens kind prefix name in 
    add_loaded (Compile.compilejs Compile.({ name ; tokens ; map ; jsargs }))

  module MakeLoader = functor (From : sig val from : string end) -> struct

    module JsHtml = functor (Def:JS_HTML_TEMPLATE_DEF) -> struct
      type t = Def.t

      let all_templates = List.map begin fun language ->
	let source   = Def.source  language in
	let mapping  = Def.mapping language in 
	let jsargs   = Def.script  language in
	let template = jsload From.from source mapping jsargs `Html in 
	(language, template) 
      end I18n.languages

      let template language =
	ListAssoc.try_get all_templates language |> BatOption.default Mk.empty
      let render t i18n ctx = 
	to_html (template (I18n.language i18n)) t i18n ctx

    end

    module Html = functor (Def:HTML_TEMPLATE_DEF) -> struct
      type t = Def.t

      let all_templates = List.map begin fun language ->
	let source   = Def.source language in
	let mapping  = Def.mapping language in 
	let template = jsload From.from source mapping [] `Html in 
	(language, template) 
      end I18n.languages

      let template language =
	ListAssoc.try_get all_templates language |> BatOption.default Mk.empty
      let render t i18n ctx = 
	to_html (template (I18n.language i18n)) t i18n ctx

    end

    module Text = functor (Def:TEXT_TEMPLATE_DEF) -> struct
      type t = Def.t

      let all_templates = List.map begin fun language ->
	let source   = Def.source language in
	let mapping  = Def.mapping language in 
	let template = load From.from source mapping `Text in 
	(language, template) 
      end I18n.languages

      let template language =
	ListAssoc.try_get all_templates language |> BatOption.default Mk.empty
      let render t i18n ctx = 
	to_text (template (I18n.language i18n)) t i18n ctx

    end

  end


end

