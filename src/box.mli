(* Jogging is © 2011 Victor Nicollet *)
(** The {b Box} layer: client-side nested AJAX boxes with server-side lazy evaluation. 

    {3 General considerations}

    This module interacts with a client page that runs the [jog] javascript mini-framework
    combined with the {{:http://www.asual.com/jquery/address/}jQuery Address plugin}, after
    [jog.init] has been called (possibly through {!val:JsBase.init}). 

    The Box module lets the server parse AJAX requests from the client and determine which 
    parts of the client page should be refreshed. This eliminates the need for sending
    shared parts of the page, and thus reduces the bandwith usage (and the processing time
    required for rendering those parts). 
    
    The underlying principle is that of a tree of boxes, each box representing an atomic
    piece of content that can be either sent or omitted. The server-side code constructs 
    such a tree (using lazy constructs) and uses the Box module to send the tree over to 
    the client. 

    To determine whether a given node should be sent or kept, the Box module uses the pieces
    of the client-side deep link URL. For instance, if the user was on address [#/foo/bar]
    and moves to address [#/foo/baz], the client will sent an AJAX request asking for [/foo/baz]
    and mention that the first segment ([foo]) was not modified. On the server side, every
    box knows which segments of the URL matter: if all of the segments used by a box have
    remained the same, it is not reconstructed (but its children might be). 

    {3 Parsing the URL segments}

    The box tree parses the URL segments it receives as input, perhaps turning a set of 
    segments like [/user/1387878/edit] into [((),"user"),1387878),"edit"]. The rule for
    turning every one of these segments into a corresponding OCaml value is described
    by the {!Seg} module below. 

    This parsing does not occur in the entire tree, however. One kind of tree node, the
    [parse] node, is able to parse one segment into one additional piece of data. All 
    other nodes expect their parent nodes to provide them with the readilly-parsed data.

    This expectation is represented by the type parameter of the node type. For instance,
    if a box expects the above [((unit * string) * int) * string] type, then its type 
    would be [ (((unit * string) * int) * string) MyeBox.t].

    This explains the type of the parse node constructor, ['a Seg.t -> ('b * 'a) t -> 'b t]:
    it takes a box that needs both ['b] and ['a], a segment parser that returns an ['a],
    and returns a box that only needs a ['b]. Multiple applications of the parse node
    constructor would allow turning complex box types into the root [unit MyBox.t] type
    that can actually be rendered.
    
    @author Victor Nicollet
    @version 0.91
*)

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

module Customize : 
  functor (Action:Action.CUSTOMIZABLE) ->
    CUSTOMIZABLE with type response = Action.response
		 and type server = Action.server
