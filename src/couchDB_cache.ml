(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives

module ImplDB     = CouchDB_database
module ImplParser = CouchDB_parser

(* Preliminary definitions for the internal types. ----------------------------------------- *)

(* An ordered module that serves as a key for the cache map. Again, it's ordered so
   that the element that changes the most often comes first.
 *)
module CacheKey = struct

  type t = {
    id : Id.t ;
    db : ImplDB.t
  }

  let compare a b =
    let c = compare a.id b.id in 
    if c <> 0 then c else
      if a.db == b.db then 0 else 
	compare a.db.ImplDB.db_id b.db.ImplDB.db_id

  let make database id = { id = id ; db = database }
  
  let url x = 
    x.db.ImplDB.db_prefix ^ Id.str x.id

end

(* An ordered module that serves as a key for the pending map. *)
module PendingKey = struct
  type t = ImplDB.t
  let compare = compare
end

module CacheMap   = Map.Make(CacheKey)
module PendingMap = Map.Make(PendingKey)
module LoadSet    = Set.Make(CacheKey)
module IdSet      = Set.Make(Id)
  
(* The exported types. ---------------------------------------------------------------------- *)

(* A cache maps identifiers to the raw JSON that was extracted from the database,
   which includes the possibility that there was no JSON in the database in the
   first place for that given identifier. Keep in mind that this is a reflection
   of the database contents, although possibly outdated ! 
*)
type cached = {
  json  : Json_type.t ;
  rev   : string option ;
  parse : ImplParser.cache
}

type t = {
  mutable cached  : cached option CacheMap.t ;
  mutable pending : IdSet.t PendingMap.t ;
  mutable loading : LoadSet.t ;
} 

class virtual ctx = object
  val couchDB = { 
    cached  = CacheMap.empty ; 
    pending = PendingMap.empty ; 
    loading = LoadSet.empty } 
  method couchDB = couchDB
  method virtual time : float
end

(* A little bit of implementation ---------------------------------------------------------- *)

(* Parse some JSON : extract the revision (if impossible, assume there is no
   revision: the situation will auto-correct when attempting to write the
   data back). *)
let cached_of_json json = 
  let rev = 
    try Some (Json_type.Browse.objekt json 
		 |> List.assoc "_rev"
		 |> Json_type.Browse.string)
    with _ -> None
  in
  { json = json ; rev = rev ; parse = ImplParser.cache () } 
    
(* Add a value to the cache. *)
let cache_values list =
  Run.bind begin fun ctx -> 
    let couchDB = ctx # couchDB in
    let cached, loading = List.fold_left 
      (fun (cached,loading) (key,value) -> 
	(CacheMap.add key value cached, LoadSet.remove key loading)) 
      (couchDB.cached,couchDB.loading) list 
    in
    couchDB.cached  <- cached ;
    couchDB.loading <- loading ;
    Run.return ()
  end Run.context

(* Fetch all pending values from a given database and store them in the cache. *)
let fetch database = 
  Run.bind begin fun ctx -> 
    let couchDB = ctx # couchDB in 
    try let ids = PendingMap.find database couchDB.pending in
	let ids = IdSet.fold (fun id l -> CacheKey.make database id :: l) ids [] in
	couchDB.loading <- 
	  List.fold_left (fun set k -> LoadSet.add k set) couchDB.loading ids ;
	couchDB.pending <- PendingMap.remove database couchDB.pending ;	
	match ids with
	  | []    -> Run.return () 
	  | [key] -> begin 
	    let url = CacheKey.url key in 
	    Util.logreq "GET %s" url ;
	    try let result = Http_client.Convenience.http_get url in
		try let json   = Json_io.json_of_string ~recursive:true result in
		    cache_values [key, Some (cached_of_json json)]		
		with 	
		  | Json_type.Json_error error ->
		    Util.log "CouchDB.get: `%s` : %s on: %s" url error result ; 
		    couchDB.loading <- LoadSet.empty ;
		    Run.return ()
	    with
	      | Http_client.Http_error (404, _) -> cache_values [key,None] 
	      | Http_client.Http_error (status, desc) ->
		Util.log "CouchDB.get: `%s` : %d %s" url status desc ; 
		couchDB.loading <- LoadSet.empty ;
		Run.return ()
	      | exn ->  
		Util.log "CouchDB.get: `%s` : %s" url (Printexc.to_string exn) ; 
		couchDB.loading <- LoadSet.empty ;
		Run.return ()
	  end 
	  | keys -> begin 
	    let keys_str = Json_io.string_of_json ~recursive:true ~compact:true 
	      (Json_type.Build.list
		 (fun key -> Id.to_json key.CacheKey.id) keys) in
	    let url = String.concat "" 
	      [ database.ImplDB.db_prefix ; "_all_docs?" ; 
		Netencoding.Url.mk_url_encoded_parameters [
		  "include_docs", "true" ; "keys", keys_str ] ] in
	    Util.logreq "GET %s" url ;
	    try let result = Http_client.Convenience.http_get url in
		try let list   = Json_io.json_of_string ~recursive:true result
		      |> Json_type.Browse.objekt
		      |> List.assoc "rows"
		      |> Json_type.Browse.array in
		    let docs   = BatList.filter_map begin fun json -> 
		      try let fields = Json_type.Browse.objekt json in 
			  let id     = Id.of_json (List.assoc "id" fields) in
			  Some (id, List.assoc "doc" fields)
		      with _ -> None 
		    end list in   
		    let values = List.map 
		      (fun key -> 
			let json_opt =
			  try Some (List.assoc key.CacheKey.id docs) with Not_found -> None in
			key, BatOption.map cached_of_json json_opt) keys in
		    cache_values values
		with 	
		  | Not_found ->
		    Util.log "CouchDB.get: `%s` : 'rows' missing on: %s" url result ; 
		    couchDB.loading <- LoadSet.empty ;
		    Run.return () 
		  | Json_type.Json_error error ->
		    Util.log "CouchDB.get: `%s` : %s on: %s" url error result ; 
		    couchDB.loading <- LoadSet.empty ;
		    Run.return ()
	    with 
	      | Http_client.Http_error (status, desc) ->
		Util.log "CouchDB.get: `%s` : %d %s" url status desc ; 
		couchDB.loading <- LoadSet.empty ;
		Run.return () 
	      | exn -> 
		Util.log "CouchDB.get: `%s` : %s" url (Printexc.to_string exn) ; 
		couchDB.loading <- LoadSet.empty ;
		Run.return ()
	  end	
    with Not_found -> Run.return () 	
  end Run.context

exception CouchDB_error

(* Get a value from the cache, fetch it from the database if it not found. *)
let rec get ?(force=false) ?(retries=5) key = 
  if retries <= 0 then raise CouchDB_error else
    Run.bind begin fun ctx -> 
      let couchDB = ctx # couchDB in 
      try Run.return (CacheMap.find key couchDB.cached) with Not_found -> 
	if LoadSet.mem key couchDB.loading then get ~force ~retries key else 
	  let ids = try PendingMap.find key.CacheKey.db couchDB.pending 
	    with Not_found -> IdSet.empty in 
	  let ids = IdSet.add key.CacheKey.id ids in 
	  couchDB.pending <- PendingMap.add key.CacheKey.db ids couchDB.pending ;
	  if force then 
	    Run.bind (fun () -> get ~force:true ~retries:(retries-1) key) 
	      (fetch key.CacheKey.db)
	  else
	    Run.yield (get ~force:true ~retries key)
    end Run.context
      
(* Get a value only if it is present in the cache. *)
let get_if_exists key = 
  Run.bind begin fun ctx -> 
    let couchDB = ctx # couchDB in
    try Run.return (Some (CacheMap.find key couchDB.cached)) with Not_found -> 
      Run.return None
  end Run.context
	    
let remove key = 
  Run.bind begin fun ctx -> 
    let couchDB = ctx # couchDB in
    couchDB.cached <- CacheMap.remove key couchDB.cached ;
    Run.return ()
  end Run.context
