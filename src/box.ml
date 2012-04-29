(* Ohm is © 2011 Victor Nicollet *)

open Util
open BatPervasives

module type CUSTOMIZABLE = sig
	  
  module Seg : sig
      
    type 'a t 
      
    val make : 'a -> (string -> 'a) -> ('a -> string) -> 'a t
      
    type 'a set 
      
    val (++) : 'a set -> 'b t -> ('a * 'b) set
      
    val root : unit set
      
    val to_url : 'a set -> 'a -> string list
      
  end
    
  type source = <
    args : int    -> string option ;
    post : string -> string option ;
    json : Json_type.t
  > ;;

  type reaction 
  type response
    
  val string_of_reaction : reaction -> string
    
  type url_builder = string list -> string
      
  type 'input box_context = <
    name : string ;
    args : 'input ;
    post : string -> string option ;
    json : Json_type.t ;
    segments     : 'input Seg.set ;
    reaction_url : reaction -> string 
  > ;;

  type ('ctx,'data) t 

  val node     :
    (   'a box_context
     -> 'a 
     -> ('ctx,(string * ('ctx,'a) t) list) Run.t * ('ctx,View.html) Run.t) ->
    ('ctx,'a) t

  val leaf     :
    (   'a box_context 
     -> 'a
     -> ('ctx,View.html) Run.t) ->
    ('ctx,'a) t

  val error    :
    (   'a box_context
     -> 'a
     -> ('ctx,JsCode.t) Run.t) ->
    ('ctx,'a) t

  val decide   : 
    (   'a box_context 
     -> 'a 
     -> ('ctx,('ctx,'a) t) Run.t) ->
    ('ctx,'a) t 

  val parse    : 'a Seg.t -> ('ctx,'b * 'a) t -> ('ctx,'b) t

  val reaction :
    string ->
    (reaction -> 'a box_context -> 'a -> response -> ('ctx,response) Run.t) ->
    (reaction -> ('ctx,'a) t) ->
    ('ctx,'a) t

  val to_js    :
    ('ctx,unit) t ->
    url_builder ->
    source ->
    int list ->
    ('ctx,JsCode.t) Run.t

  val on_update  :
    ('ctx,unit) t -> 
    url_builder -> 
    source -> 
    int list ->
    response ->
    ('ctx,response) Run.t

  val on_reaction  : 
    ('ctx,unit) t -> url_builder -> source -> response -> ('ctx,response) Run.t

  val draw_container : string * string -> View.html
    
  val root    : string * string
    
  type server
    
  class type ['a] root_action = object 
    method server : server
    method path : string
    method initial : 'a -> string
  end
	    
  class ['a] controller : 'a root_action -> string list -> object
    method server : server
    method path   : string
    method rest   : 'a -> string list -> string
  end

end

module Customize = 
  functor (Action:Action.CUSTOMIZABLE) ->
struct

  module Seg = struct
      
    type 'data t = 
	{
	  default   : 'data ;
	  of_string : string -> 'data ;
	  to_string : 'data  -> string ;
	}
	  
    let make default of_string to_string = 
      {
	default   = default ;
	of_string = of_string ;
	to_string = to_string ;
      }
	
    let parse seg = function
      | None   -> seg.default 
      | Some s -> seg.of_string s
	
    type 'dataset set = 
	{
	  prepend : 'dataset -> string list -> string list ;
	}
	  
    let root = { prepend = fun () list -> list }
      
    let (++) set t = 
      {
	prepend = fun (sdata, tdata) list ->
	  set.prepend sdata (t.to_string tdata :: list)
      }      
	
    let to_url set data = set.prepend data [] 
      
  end
    
  type source = <
    args : int    -> string option ;
    post : string -> string option ;
    json : Json_type.t ;
  > ;;

  type url_builder = string list -> string
      
  type reaction = string
  let string_of_reaction = identity
    
  type 'input _source = 
      {
	parsed    : 'input ;
	segments  : 'input Seg.set ; 
	unparsed  : int -> string option ;
	post      : string -> string option ;
	json      : Json_type.t Lazy.t ;
	next      : int ;
	same      : int list ;
	builder   : url_builder 
      }
	
  let _make_source source depend builder = {
    parsed   = () ;
    segments = Seg.root ;
    unparsed = source # args ;
    next     = 0 ;
    post     = source # post ;
    json     = lazy (source # json) ;
    same     = List.sort compare depend ;
    builder  = builder ;
  }
    
  type 'input box_context = <
    name : string ;
    args : 'input ;
    post : string -> string option ;
    json : Json_type.t ;
    segments : 'input Seg.set ;
    reaction_url : reaction -> string 
  > ;;

  let input _nam _src = object 
      
    val nam = _nam
    val src = _src 
      
    method name = nam
    method args = src.parsed
    method post = src.post 
    method json = Lazy.force src.json
    method segments = src.segments
    method reaction_url name = src.builder (Seg.to_url src.segments src.parsed @ [nam^"."^name])
      
  end
    
  let draw_container (prefix,name) ctx = 
    let name = if prefix = "" then name else prefix ^ "." ^ name in
    let id = Id.gen () in 
    ctx
    |> View.str ("<div id=\""^Id.str id^"\"></div>")
    |> View.Context.add_js_code (JsBase.defineBox name id) 
  
  let root = "", "root"

  type response = Action.response

  type ('ctx,'input) t = {
    do_force : string -> 'input _source -> ('ctx,JsCode.t) Run.t ;
    do_lazy  : string -> 'input _source -> ('ctx,JsCode.t) Run.t ;
    do_post  : string -> string -> 'input _source -> response -> ('ctx,response option) Run.t ;
  }

  let node content = 
    let do_force nam src =
      let boxes, code  = content (input nam src) src.parsed in
      boxes |> Run.bind (fun boxes -> 
	code |> Run.bind (fun code -> 
	  Run.bind 
	    (fun list -> Run.return (JsCode.seq (JsBase.fillBox nam code :: list)))
	    (List.fold_left 
	       (fun acc (str,box) -> 
		 acc |> Run.bind (fun acc ->
		   (box.do_force (nam^"."^str) src) |> Run.bind (fun js -> 
		     Run.return (js :: acc)
		   )
		 )
	       )
	       (Run.return [])  boxes)
	)
      )	      	
    in
    let do_lazy nam src = 
      let boxes, _ = content (input nam src) src.parsed in
      boxes |> Run.bind (fun boxes ->
	Run.bind
	  (fun list -> Run.return (JsCode.seq list))	        
	  (List.fold_left
	     (fun acc (str,box) -> 
	       acc |> Run.bind (fun acc ->
		 (box.do_lazy (nam^"."^str) src) |> Run.bind (fun js -> 
		   Run.return (js :: acc)
		 )
	       )
	     )
	     (Run.return []) boxes)
      )
    in
    let do_post nam act src res = 
      let boxes, _ = content (input nam src) src.parsed in
       boxes |> Run.bind (fun boxes ->
	List.fold_left
	  (fun acc (str,box) -> 
	    acc |> Run.bind (function 
	      | Some res -> Run.return (Some res)
	      | None     -> box.do_post (nam^"."^str) act src res))
	  (Run.return None) boxes
      )
    in
    { do_force = do_force ; do_lazy = do_lazy ; do_post = do_post }
      
  let leaf code = 
    let do_force nam src =       
      let content = code (input nam src) src.parsed in
       content |> Run.bind (fun code ->
	Run.return (JsBase.fillBox nam code)
      )
    in 
    let do_lazy nam src = 
      Run.return (JsCode.seq [])
    in
    let do_post _ _ _ _ = Run.return None in
    { do_force = do_force ; do_lazy = do_lazy ; do_post = do_post } 
      
  let error code = 
    let do_it nam src = 
      code (input nam src) src.parsed
    in
    let do_post _ _ _ _ = Run.return None in 
    { do_force = do_it ; do_lazy = do_it ; do_post = do_post }
      
  let decide f = 
    let do_force nam src = 
      Run.bind (fun r -> r.do_force nam src) (f (input nam src) src.parsed)
    in
    let do_lazy nam src = 
      Run.bind (fun r -> r.do_lazy nam src) (f (input nam src) src.parsed) 
    in
    let do_post nam act src res = 
      Run.bind (fun r -> r.do_post nam act src res) (f (input nam src) src.parsed)
    in
    { do_force = do_force ; do_lazy = do_lazy ; do_post = do_post }
      
  let parse seg box = 
    let (++) = Seg.(++) in
    let do_force nam src = 
      box.do_force nam { 
	parsed   = ( src.parsed , Seg.parse seg (src.unparsed src.next) ) ;
	segments = src.segments ++ seg ;
	unparsed = src.unparsed ;
	post     = src.post ;
	json     = src.json ;
	next     = src.next + 1 ;
	same     = src.same ;
	builder  = src.builder 
      }
    in
    let do_lazy nam src = 
      let new_same = match src.same with h :: t when src.next = h -> Some t | _ -> None in
      match new_same with None -> do_force nam src | Some new_same ->
	box.do_lazy nam {
	  parsed   = ( src.parsed , Seg.parse seg (src.unparsed src.next) ) ;
	  segments = src.segments ++ seg ;
	  unparsed = src.unparsed ;
	  post     = src.post ;
	  json     = src.json ;
	  next     = src.next + 1;
	  same     = new_same ;
	  builder  = src.builder
	}
    in
    let do_post nam act src res = 
      box.do_post nam act {
	parsed   = ( src.parsed , Seg.parse seg (src.unparsed src.next) ) ;
	segments = src.segments ++ seg ;
	unparsed = src.unparsed ;
	post     = src.post ;
	json     = src.json ; 
	next     = src.next + 1 ;
	same     = src.same ;
	builder  = src.builder ;
      } res
    in      
    { do_force = do_force ; do_lazy = do_lazy ; do_post = do_post }
      
  let reaction name action callback = 
    let box = callback name in 
    let do_post nam act src res = 
      if act = nam ^ "." ^ name then 
	Run.bind (fun res -> Run.return (Some res)) (action name (input nam src) src.parsed res)
      else 
	box.do_post nam act src res
    in
    { do_force = box.do_force ; do_lazy = box.do_lazy ; do_post = do_post }

  let to_js box builder source depend = 
    box.do_lazy (snd root) (_make_source source depend builder)
      
  let on_update box builder source depend response = 
    Run.bind (fun js ->
      Run.return (Action.javascript js response)
    ) (to_js box builder source depend) 
      
  let on_reaction box builder source response =
    let rec last acc n = 
      match source # args n with Some x -> last x (n+1) | None -> acc
    in
    let action = last "" 0 in
    Run.bind 
      (function 
	| Some response -> Run.return response
	| None          -> Run.return response)
      (box.do_post (snd root) action (_make_source source [] builder) response)     

  type server = Action.server
 
  class type ['a] root_action = object 
    method server : server
    method path : string
    method initial : 'a -> string
  end
	    
  class ['a] controller (root:'a root_action) path = object (self)
      
    val root       = root
    val ajax_path  = path
    val ajax_count = List.length path
      
    method server = root # server
    method path   = root # path ^ "/" ^ String.concat "/" ajax_path
      
    method rest arg list = 
      (root # initial arg) ^ "/#/"
    ^ (String.concat "/" ajax_path) 
    ^ "/" ^ (String.concat "/" (List.filter (fun s -> s <> "") list))
	
  end
            
end
