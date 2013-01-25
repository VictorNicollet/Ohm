(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives

module ImplParser = CouchDB_parser
module ImplTypes  = CouchDB_types
module ImplCache  = CouchDB_cache
module ImplDB     = CouchDB_database

(* Small utility functions for Json field manipulation. *)

let json_replace field value obj = 
  try Json.to_assoc obj
      |> BatList.remove_assoc field
      |> BatList.cons (field, value)
      |> Json.of_assoc
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

  let using id f = 
    Run.map begin function 
      | None -> None
      | Some json -> Some (f json) 
    end (get id) 

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
      Json.serialize (Json.String k) in

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
	try let list = 
	      Json.unserialize json_str 
	      |> Json.to_object (fun ~opt ~req -> Json.to_array (req "rows"))
	      |> List.map (Json.to_object (fun ~opt ~req -> Id.of_json (req "id")))
	    in 
	    Ok list	    
	with Json.Error error as exn ->
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
    
  let all_ids ~count start = 

    let no_design_docs list = 
      List.filter (fun id -> not (BatString.starts_with (Id.str id) "_design/")) list
    in

    let limit = count + 1 in
    let list = BatStd.ok (query_all_docs start limit) in
    let first, next = try BatList.split_at count list with _ -> list, [] in
    let next = match next with h :: _ -> Some h | [] -> None in
    let first = no_design_docs first in 

    Run.return (first,next)

  module Raw = struct
      
    let put id json = 
      
      let key = ImplCache.CacheKey.make database id in
      let url = ImplCache.CacheKey.url key in
      
      (* Extract previously available data from the cache, if any. *)
      ImplCache.get_if_exists key |> Run.bind begin fun cached -> 
	
	let rev, ct = 
	  match cached with None | Some None -> None, None | Some (Some cached) ->
	    let rev = cached.ImplCache.rev in
	    let ct = 
	      try Some (Json.to_object (fun ~opt ~req -> req "ct") cached.ImplCache.json)
	      with _ -> None
	    in 
	    rev, ct
	in
	
	(* Keep "ut" and "ct" timers on every object for debugging *)
	Run.context |> Run.bind begin fun ctx ->
	  
	  let update_time = Json.String (Util.string_of_time (ctx # time)) in
	  let create_time = BatOption.default update_time ct in
	  
	  (* The JSON to be written to the database. *)
	  let json = 
	    json
  	    |> json_replace "ct" create_time
	    |> json_replace "ut" update_time
	    |> (match rev with 
		| None -> identity 
		| Some rev -> json_replace "_rev" (Json.String rev))
	    |> json_replace "_id" (Id.to_json id) 
	  in
	  
	  (* Send the new document to the database now. *)
	  
	  let json_str = Json.serialize json in
	  
	  let rec retry retries = 
	    try Util.logreq "PUT %s %s" url json_str ;
		let response = Http_client.Convenience.http_put url json_str in
		try let rev = 
		      Json.unserialize response
                      |> Json.to_object (fun ~opt ~req -> Json.to_string (req "rev"))
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
	    | None -> Run.bind (fun () -> Run.return `ok) (ImplCache.cache_values [key,None])
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
	
  end
	  
  let rec create elt = 
    let id = Id.gen () in
    Raw.put id elt |> Run.bind (function 
      | `collision -> create elt
      | `ok -> Run.return id)
	
  type ('ctx,'a) update = elt option -> ('ctx,'a * [`put of elt | `keep | `delete]) Run.t

  let transact id update = 
    Raw.transaction id (get |- Run.bind update)

  let ensure id eval = 
    transact id (function 
      | Some obj -> Run.return (obj,`keep)
      | None     -> let obj = Lazy.force eval in 
		    Run.return (obj,`put obj))

  let delete id = 
    Raw.transaction id (fun _ -> Run.return ((),`delete))

  let delete_if id pred = 
    transact id (function 
      | Some e when pred e -> Run.return ((),`delete)
      | _ -> Run.return ((),`keep))

  let update id f = 
    transact id (function 
      | Some e -> Run.return ((),`put (f e))
      | None   -> Run.return ((),`keep))

  let replace id f = 
    transact id (fun e -> Run.return ((),`put (f e)))

  let set id elt = 
    Raw.transaction id (fun _ -> Run.return ((),`put elt))

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

  let using id f = 
    Run.map begin function 
      | None -> None
      | Some elt -> Some (f elt)
    end (get id) 

  let parse id p = Database.parse (Id.to_id id) p

  let all_ids ~count start = 
    Run.map 
      (fun (list, next) -> List.map Id.of_id list, BatOption.map Id.of_id next)
      (Database.all_ids ~count (BatOption.map Id.to_id start))

end


module Table = 
  functor (Database:ImplTypes.DATABASE) -> 
    functor (Id:ImplTypes.ID) -> 
      functor (Type:Fmt.FMT) -> 
struct

  include ReadTable(Database)(Id)(Type)

  module Raw = struct

    let put id elt = Database.Raw.put (Id.to_id id) (Type.to_json elt) 
    let delete  id = Database.Raw.delete (Id.to_id id) 

    let transaction id update = 
      Database.Raw.transaction (Id.to_id id) begin fun id ->
	let translate (result,action) = 
	  let action = match action with
	    | `keep -> `keep
	    | `delete -> `delete
	    | `put elt -> `put (Type.to_json elt)
	  in ( result, action )
	in
	Run.map translate (update (Id.of_id id))
      end

  end
 
  type ('ctx,'a) update = elt option -> ('ctx,'a * [`put of elt | `keep | `delete]) Run.t
  
  let transact id update = 

    let update json = 
      Run.map 
	(function 
	  | x, `put elt -> (x,`put (Type.to_json elt))
	  | x, `keep    -> (x,`keep)
	  | x, `delete  -> (x,`delete))
	(update (BatOption.bind Type.of_json_safe json)) 
    in

    Database.transact (Id.to_id id) update

  let create elt = 
    Run.map Id.of_id (Database.create (Type.to_json elt))

  let ensure id eval = 
    transact id (function 
      | Some obj -> Run.return (obj,`keep)
      | None     -> let obj = Lazy.force eval in 
		    Run.return (obj,`put obj))
  let delete id = 
    Database.delete (Id.to_id id) 

  let delete_if id pred = 
    Database.delete_if (Id.to_id id) begin fun json -> 
      match Type.of_json_safe json with 
	| Some t -> pred t
	| None -> false
    end 

  let update id f = 
    Database.update (Id.to_id id) begin fun json -> 
      match Type.of_json_safe json with 
	| Some t -> Type.to_json (f t)
	| None   -> json 
    end 

  let replace id f = 
    Database.replace (Id.to_id id) begin fun json_opt -> 
      let t_opt = BatOption.bind Type.of_json_safe json_opt in
      Type.to_json (f t_opt) 
    end

  let set id elt = 
    Database.set (Id.to_id id) (Type.to_json elt) 
           
end
