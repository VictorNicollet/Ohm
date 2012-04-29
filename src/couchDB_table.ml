(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives

module ImplParser = CouchDB_parser
module ImplTypes  = CouchDB_types
module ImplCache  = CouchDB_cache
module ImplDB     = CouchDB_database

(* Small utility functions for Json field manipulation. *)

let json_replace field value obj = 
  try Json_type.Browse.objekt obj
      |> BatList.remove_assoc field
      |> BatList.cons (field, value)
      |> Json_type.Build.objekt
  with _ -> obj 

module Database = functor (Config:ImplTypes.CONFIG) -> struct
    
  let database = ImplDB.make_database
    ~host:Config.host
    ~port:Config.port
    ~database:Config.database
    
  type id  = Id.t
  type elt = Json_type.t

  let get id = 
    Run.bind begin function 
      | None     -> Run.return None
      | Some doc -> Run.return (Some doc.ImplCache.json) 
    end (ImplCache.get (ImplCache.CacheKey.make database id))

  let parse id elt_parser = 
    Run.bind begin function 
      | None     -> Run.return None
      | Some doc -> match ImplCache.(ImplParser.read elt_parser doc.json doc.parse) with 
	  | None -> Run.return None
	  | Some elt -> Run.return (Some elt)  
    end (ImplCache.get (ImplCache.CacheKey.make database id))

  let rec query_all_docs ?(retries=5) start limit = 

    let retry exn = 
      if retries <= 0 then Bad exn else query_all_docs ~retries:(retries-1) start limit in

    let key k = 
      Json_io.string_of_json ~recursive:true ~compact:true (Json_type.String k) in

    let keep = function (x, None) -> None | (x, Some y) -> Some (x,y) in
    let args = BatList.filter_map keep [
      "limit",          Some (string_of_int limit) ;
      "startkey",       BatOption.map (Id.str |- key) start ;
    ] in
    
    let url = 
      String.concat "" [
	database.ImplDB.db_prefix ;
	"_all_docs" ;
	"?" ;
	Netencoding.Url.mk_url_encoded_parameters args
      ]
    in

    try Util.logreq "GET %s" url ;
	let json_str = Http_client.Convenience.http_get url in
	try let list = Json_type.Browse.(
	      Json_io.json_of_string ~recursive:true json_str 
	      |> (objekt |- List.assoc "rows" |- array)
	      |> List.map (objekt |- List.assoc "id" |- string |- Id.of_string)
	    ) in	  
	    Ok list	    
	with Json_type.Json_error error as exn ->
	  Util.log "CouchDB : `%s` : `%s` on: %s" url error json_str ;
	  retry exn
    with 
      | Http_client.Http_error (status,desc) as exn ->
	Util.log "CouchDB : `%s` : %d %s`" url status desc ;
	retry exn
      | Http_client.Http_protocol (exn') as exn ->
	Util.log "CouchDB : `%s` : HTTP Error %s" url (Printexc.to_string exn') ;
	retry exn 
      | exn -> 
	Util.log "CouchDB : `%s` : Error %s" url (Printexc.to_string exn) ;
	retry exn 
    
  let all_ids () = 

    let no_design_docs list = 
      List.filter (fun id -> not (BatString.starts_with (Id.str id) "_design/")) list
    in

    let rec fetch_all_identifiers next =
      let limit  = 500 in
      let list   = BatStd.ok (query_all_docs next limit)in 
      let length = List.length list in 
      if length < limit then Run.return [no_design_docs list] else
	match List.rev list with 
	  | last :: others -> Run.bind begin fun tail -> 
         	                Run.return (no_design_docs others :: tail) 
                              end (fetch_all_identifiers (Some last))
	  | []             -> Run.return []
    in

    Run.of_call fetch_all_identifiers None |> Run.map List.concat

  let put id json = 

    let key = ImplCache.CacheKey.make database id in
    let url = ImplCache.CacheKey.url key in

    (* Extract previously available data from the cache, if any. *)
    ImplCache.get_if_exists key |> Run.bind begin fun cached -> 

      let rev, ct = 
	match cached with None | Some None -> None, None | Some (Some cached) ->
	  let rev = cached.ImplCache.rev in
	  let ct = 
	    try let obj = Json_type.Browse.objekt cached.ImplCache.json in    
		Some (List.assoc "ct" obj)
	    with _ -> None
	  in 
	  rev, ct
      in

      (* Keep "ut" and "ct" timers on every object for debugging *)
      Run.context |> Run.bind begin fun ctx ->

	let update_time = Json_type.String (Util.string_of_time (ctx # time)) in
	let create_time = BatOption.default update_time ct in

	(* The JSON to be written to the database. *)
	let json = 
	  json
  	  |> json_replace "ct" create_time
	  |> json_replace "ut" update_time
	  |> (match rev with 
	    | None -> identity 
	    | Some rev -> json_replace "_rev" (Json_type.Build.string rev))
	  |> json_replace "_id" (Id.to_json id) 
	in

	(* Send the new document to the database now. *)
    
	let json_str = Json_io.string_of_json ~compact:true ~recursive:true json in

	let rec retry retries = 
	  try Util.logreq "PUT %s %s" url json_str ;
	      let response = Http_client.Convenience.http_put url json_str in
	      try let rev = 
		    Json_io.json_of_string ~recursive:true response
                    |> Json_type.Browse.objekt
  		    |> List.assoc "rev"
		    |> Json_type.Browse.string
		  in Run.return (`ok (Some rev))
	      with _ -> Run.return (`ok None)
	  with 
	    | Http_client.Http_error (409,_) ->Run.return `collision
	    | Http_client.Http_error (status,desc) as exn ->
	      Util.log "CouchDB.put: `%s %s` : %d %s" url json_str status desc ;
	      if retries <= 0 then raise exn else retry (retries-1)
	    | Http_client.Http_protocol error as exn ->
	      Util.log "CouchDB.put: HTTP error (%s) on %s\n%s" (Printexc.to_string error)
		url json_str ;
	      if retries <= 0 then raise exn else retry (retries-1)
	    | exn -> 
	      if retries <= 0 then raise exn else retry (retries-1)
		
	in
	
	retry 5 |> Run.bind begin function 
	  | `collision -> Run.bind (fun () -> Run.return `collision) (ImplCache.remove key) 
	  | `ok rev    -> Run.bind (fun () -> Run.return `ok) 
            (ImplCache.cache_values [key,Some (ImplCache.cached_of_json json)])
	end
      end
    end
	
  type ('ctx,'a) update = id -> ('ctx,'a * [`put of elt | `keep | `delete]) Run.t

  let delete id = 
    
    let key = ImplCache.CacheKey.make database id in
    
    let rec remove ?(retries=5) rev = 
      let url = ImplCache.CacheKey.url key ^ "?rev=" ^ rev in
      try Util.logreq "DELETE %s" url ;
	  ignore (Http_client.Convenience.http_delete url) ;
	  Run.bind (fun () -> Run.return `ok) (ImplCache.cache_values [key,None])
      with 
	| Http_client.Http_error (409,_) -> 
	  Run.bind (fun () -> Run.return `collision) (ImplCache.remove key)
	| Http_client.Http_error (status,desc) as exn ->
	  Util.log "CouchDB.delete: `%s` : %d %s" url status desc ;
	  if retries <= 0 then raise exn else remove ~retries:(retries-1) rev
	| exn -> 
	  Util.log "CouchDB.delete : `%s` : %s" url (Printexc.to_string exn) ;
	  if retries <= 0 then raise exn else remove ~retries:(retries-1) rev

    in

    ImplCache.get key |> Run.bind begin function
      | None     -> Run.return `ok 
      | Some doc -> match doc.ImplCache.rev with 
	  | None     -> Run.bind (fun () -> Run.return `ok) (ImplCache.cache_values [key,None])
	  | Some rev -> remove rev
    end

  let transaction id update = 
    
    let rec loop retries = 
      if retries <= 0 then raise ImplCache.CouchDB_error else
	update id |> Run.bind begin fun (returned,operation) -> 

	  let confirm action = 
	    action |> Run.bind begin function 
	      | `ok        -> Run.return returned
	      | `collision -> Run.bind (fun _ -> loop (retries-1)) (get id)
	    end
	  in
	 
	  match operation with 
	    | `keep    -> Run.return returned
	    | `put doc -> confirm (put id doc)
	    | `delete  -> confirm (delete id)
	end
    in
    
    (* Try the transaction this many times *)
    loop 10

  (* These are mere shortcuts that don't depend on the nature of the
     monad. You could write them without knowing how the monad is implemented. *)
      
  let insert elt =
    let m = Run.return (elt, `put elt) in
    fun _ -> m

  let remove id = 
    Run.map (fun e -> e, `delete) (get id)
     
  let update f id = 
    let apply = function 
      | None -> None, `keep
      | Some e -> let e' = f e in
		  Some e', `put e'
    in
    Run.map apply (get id)

  let ensure elt id = 
    let ensure = function
      | None -> let elt = Lazy.force elt in elt, `put elt
      | Some e -> e, `keep
    in
    Run.map ensure (get id)

  let remove_if cond id = 
    let rm opt =
      opt, match opt with 
	| None -> `keep
	| Some e -> if cond e then `delete else `keep
    in
    Run.map rm (get id)

  let if_exists f id = 
    let act = function
      | None -> None, `keep
      | Some e -> let r, o = f e in Some r, o
    in
    Run.map act (get id)

end

(* The table module is a set of wrapper around the database functions. It does not
   perform any low-level access to either CouchDB or the monad. It could possibly be
   implemented without any knowledge of the monad implementation.
*)

module ReadTable = 
  functor (Database:ImplTypes.DATABASE) -> 
    functor (Id:ImplTypes.ID) -> 
      functor (Type:Fmt.READ_FMT) -> 
struct

  type id = Id.t
  type elt = Type.t
    
  let database = Database.database

  let elt_parser = ImplParser.make Type.of_json_safe

  let get id = Database.parse (Id.to_id id) elt_parser

  let parse id p = Database.parse (Id.to_id id) p

  let all_ids () = Run.map (List.map Id.of_id) (Database.all_ids ())

end


module Table = 
  functor (Database:ImplTypes.DATABASE) -> 
    functor (Id:ImplTypes.ID) -> 
      functor (Type:Fmt.FMT) -> 
struct

  include ReadTable(Database)(Id)(Type)

  let put id elt = 
    Database.put (Id.to_id id) (Type.to_json elt) 

  let delete id = Database.delete (Id.to_id id) 

  type ('ctx,'a) update = id -> ('ctx,'a * [`put of elt | `keep | `delete]) Run.t

  let transaction id update = 
    Database.transaction (Id.to_id id) begin fun id ->
      let translate (result,action) = 
	let action = match action with
	  | `keep -> `keep
	  | `delete -> `delete
	  | `put elt -> `put (Type.to_json elt)
	in ( result, action )
      in
      Run.map translate (update (Id.of_id id))
    end

  (* These are mere shortcuts that don't depend on the nature of the
     monad. You could write them without knowing how the monad is implemented. *)
      
  let insert elt =
    let m = Run.return (elt, `put elt) in
    fun _ -> m

  let remove id = 
    Run.map (fun e -> e, `delete) (get id)

  let update f id = 
    let apply = function 
      | None -> None, `keep
      | Some e -> let e' = f e in
		  Some e', `put e'
    in
    Run.map apply (get id)

  let ensure elt id = 
    let ensure = function
      | None -> let elt = Lazy.force elt in elt, `put elt
      | Some e -> e, `keep
    in
    Run.map ensure (get id)      

  let remove_if cond id = 
    let rm opt =
      opt, match opt with 
	| None -> `keep
	| Some e -> if cond e then `delete else `keep
    in
    Run.map rm (get id)

  let if_exists f id = 
    let act = function
      | None -> None, `keep
      | Some e -> let r, o = f e in Some r, o
    in
    Run.map act (get id)

end
