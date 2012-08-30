(* Ohm is © 2012 Victor Nicollet *)

exception CouchDB_Error

type implementation

class virtual ctx : object ('self) 
  method couchDB : implementation
  method virtual time : float 
end

val ctx_decay : #ctx -> ctx

class init_ctx : object
  method couchDB : implementation
  method time    : float 
end

type database

module type CONFIG = sig
  val host     : string
  val port     : int
  val database : string
end

module Parser : sig 
  type 'a t 
end

module type READ_TABLE = sig

  val database : database

  type id 
  type elt

  val get : id -> (#ctx, elt option) Run.t

  val using : id -> (elt -> 'a) -> (#ctx,'a option) Run.t 

  val parse : id -> 'a Parser.t -> (#ctx,'a option) Run.t

  val all_ids : count:int -> id option -> (#ctx,id list * id option) Run.t

end

module type TABLE = sig

  include READ_TABLE

  val create : elt -> (#ctx,id) Run.t

  val ensure : id -> elt Lazy.t -> (#ctx,elt) Run.t

  val delete : id -> (#ctx,unit) Run.t
  val delete_if : id -> (elt -> bool) -> (#ctx,unit) Run.t

  val update : id -> (elt -> elt) -> (#ctx,unit) Run.t

  val set : id -> elt -> (#ctx,unit) Run.t

  module Raw : sig
    val put    : id -> elt -> (#ctx,[> `ok | `collision]) Run.t
    val delete : id ->        (#ctx,[> `ok | `collision]) Run.t
    val transaction : 
         id 
      -> (id -> (#ctx as 'ctx,'a * [`put of elt | `keep | `delete]) Run.t)
      -> ('ctx,'a) Run.t
  end

  type ('ctx,'a) update = elt option -> ('ctx,'a * [`put of elt | `keep | `delete]) Run.t 

  val transact : id -> (#ctx as 'ctx,'a) update -> ('ctx,'a) Run.t

end

module type DATABASE = TABLE with type id = Id.t and type elt = Json_type.t

module Database :
  functor (Config:CONFIG) -> 
    DATABASE

module type ID = sig
  type t 
  val to_id : t -> Id.t
  val of_id : Id.t -> t
end

module Table : 
  functor (Database:DATABASE) -> 
    functor (Id:ID) -> 
      functor (Type:Fmt.FMT) ->
	TABLE with type id = Id.t and type elt = Type.t

module ReadTable : 
  functor (Database:DATABASE) -> 
    functor (Id:ID) -> 
      functor (Type:Fmt.READ_FMT) ->
	READ_TABLE with type id = Id.t and type elt = Type.t

module type DESIGN = sig
  module Database : DATABASE
  val name : string
end

module type MAP_DEF = sig

  module Key    : Fmt.FMT
  module Value  : Fmt.READ_FMT

  module Design : DESIGN
  val name : string

  val map : string

end

module type DOC_DEF = sig
  include MAP_DEF
  module Doc : Fmt.READ_FMT
end

module type REDUCE_DEF = sig

  include MAP_DEF

  val reduce : string
  val group  : bool
  val level  : int option

end

module type MAP_VIEW = sig

  type map_key 
  type map_value

  type map_iv = <
    id : Id.t ;
    value : map_value
  > ;;

  val by_key : map_key -> (#ctx, map_iv list) Run.t

  type map_kiv = <
    id : Id.t ;
    key : map_key ;
    value : map_value
  >

  val query : 
       ?startkey:map_key
    -> ?startid:Id.t
    -> ?endkey:map_key
    -> ?endid:Id.t
    -> ?limit:int
    -> ?descending:bool
    -> ?endinclusive:bool
    -> unit 
    -> (#ctx, map_kiv list) Run.t

end

module MapView :
  functor(Def:MAP_DEF) -> 
    MAP_VIEW with type map_key   = Def.Key.t
	     and  type map_value = Def.Value.t

module type DOC_VIEW = sig

  type doc_key 
  type doc_value
  type doc_doc

  type doc_ivd = <
    id : Id.t ;
    value : doc_value ;
    doc : doc_doc
  >

  val doc : doc_key -> (#ctx, doc_ivd list) Run.t

  type doc_kivd = <
    key : doc_key ;
    id : Id.t ;
    value : doc_value ;
    doc : doc_doc
  >

  val doc_query :
       ?startkey:doc_key
    -> ?startid:Id.t
    -> ?endkey:doc_key
    -> ?endid:Id.t
    -> ?limit:int
    -> ?descending:bool
    -> ?endinclusive:bool
    -> unit 
    -> (#ctx, doc_kivd list) Run.t

  val doc_query_first :  
       ?startkey:doc_key
    -> ?startid:Id.t
    -> ?endkey:doc_key
    -> ?endid:Id.t
    -> ?descending:bool
    -> ?endinclusive:bool
    -> unit 
    -> (#ctx, doc_kivd option) Run.t

end

module DocView : 
  functor(Def:DOC_DEF) ->
    DOC_VIEW with type doc_key   = Def.Key.t 
	     and  type doc_value = Def.Value.t
	     and  type doc_doc   = Def.Doc.t

module type REDUCE_VIEW = sig

  type reduce_key
  type reduce_value  
    
  val reduce : reduce_key -> (#ctx, reduce_value option) Run.t

  val reduce_query : 
       ?startkey:reduce_key
    -> ?endkey:reduce_key
    -> ?limit:int
    -> ?endinclusive:bool
    -> unit
    -> (#ctx, (reduce_key * reduce_value) list) Run.t

end

module ReduceView : 
  functor (Def:REDUCE_DEF) ->
    REDUCE_VIEW with type reduce_key   = Def.Key.t
		and  type reduce_value = Def.Value.t    

val compact : unit -> unit
val compile_views : unit -> unit

module Convenience : sig

  module type LOCAL_CONFIG = sig
    val db : string
  end

  module Config   : functor(Config:LOCAL_CONFIG) -> CONFIG
  module Database : functor(Config:LOCAL_CONFIG) -> DATABASE
  module Table    : 
    functor (Config:LOCAL_CONFIG) ->
      functor (Id:ID) -> 
	functor (Type:Fmt.FMT) ->
  sig
    module Tbl    : TABLE with type id = Id.t and type elt = Type.t
    module Design : DESIGN
  end

end
