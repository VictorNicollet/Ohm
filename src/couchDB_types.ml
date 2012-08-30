(* Ohm is © 2012 Victor Nicollet *)

class virtual ctx = CouchDB_cache.ctx

module type CONFIG = sig
  val host     : string
  val port     : int
  val database : string
end

module type READ_TABLE = sig

  val database : CouchDB_database.t

  type id 
  type elt

  val get : id -> (#ctx, elt option) Run.t
  val using : id -> (elt -> 'a) -> (#ctx,'a option) Run.t 
  val parse : id -> 'a CouchDB_parser.t -> (#ctx,'a option) Run.t

  val all_ids : count:int -> id option -> (#ctx,id list * id option) Run.t

end

module type TABLE = sig

  include READ_TABLE

  val create : elt -> (#ctx,id) Run.t

  val ensure : id -> elt Lazy.t -> (#ctx,elt) Run.t

  val delete : id -> (#ctx,unit) Run.t
  val delete_if : id -> (elt -> bool) -> (#ctx,unit) Run.t

  val replace : id -> (elt option -> elt) -> (#ctx,unit) Run.t
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

module type ID = sig
  type t 
  val to_id : t -> Id.t
  val of_id : Id.t -> t
end

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
