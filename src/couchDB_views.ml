(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives

module ImplParser = CouchDB_parser
module ImplTypes  = CouchDB_types
module ImplCache  = CouchDB_cache
module ImplDB     = CouchDB_database

module Views = struct

  let designs = Hashtbl.create 30

  let register ~database ~design ~name ~map ~reduce = 
    let views = 
      try Hashtbl.find designs (database,design) 
      with Not_found ->
	let views = Hashtbl.create 10 in
	Hashtbl.add designs (database,design) views ; views
    in
    let current =
      try Some (Hashtbl.find views name)
      with Not_found -> None
    in
    let with_reduce, previous_reduce = 
      (* This may be a second part of the re-definition, so keep
	 previous values to avoid overwriting them. *)
      match current with
	| Some (_,reduce,with_reduce) -> with_reduce, reduce
	| None -> ref false, None
    in
    let reduce = 
      (* We added a reduce function, so update the "with_reduce"
	 references so that map views modules know they need to
	 provide a ?reduce=false argument in the URL
      *)
      if reduce <> None then (with_reduce := true ; reduce) 
      else previous_reduce
    in
    Hashtbl.add views name (map,reduce,with_reduce) ;
    with_reduce

  module JSON = Json_type.Build

  let compile () = 
    designs 
    |> Hashtbl.iter begin fun (database,design) views ->

      (* Prepare data for writing *)

      let id  = "_design/" ^ design in
      let url = database.ImplDB.db_prefix ^ id in 

      let views = 	
	Hashtbl.fold begin fun name (map,reduce,_) list ->
	  Util.log "CouchDB: compile: %s/_design/%s/_view/%s" 
	    database.ImplDB.db_name design name ;
	  let map    = JSON.string ("(function(doc){"^map^"})") in
	  let reduce =
	    JSON.optional 
	      (fun s -> JSON.string ("(function(keys,values,rereduce){"^s^"})"))
	      reduce
	  in
	  let options = JSON.objekt ["collation", JSON.string "raw"] in
	  ( name, JSON.objekt [
	    "map",     map ;
	    "reduce",  reduce ;
	    "options", options ]) :: list
	end views []
      in

      let put = 
	[ "_id",      JSON.string id ;
	  "views",    JSON.objekt views ;
	  "language", JSON.string "javascript" ]
      in

      let rec aux rev =
	try let put = 
	      match rev with 
		| None     -> put
		| Some rev -> ( "_rev" , rev ) :: put
	    in
	    let json = JSON.objekt put in
	    let json_str = Json_io.string_of_json ~compact:true ~recursive:true json in
	    Util.logreq "PUT %s %s" url json_str ;
	    let response = Http_client.Convenience.http_put url json_str in
	    ignore response
	with 
	  |  Http_client.Http_protocol error ->
	    Util.log "CouchDB: HTTP error : %s" (Printexc.to_string error)	    
	  | Http_client.Http_error (409,_) -> 
	    Util.logreq "GET %s" url ;
	    let response = Http_client.Convenience.http_get url in
	    let rev = 
	      Json_io.json_of_string ~recursive:true response
              |> Json_type.Browse.objekt
  	      |> List.assoc "_rev"
	    in aux (Some rev)
      in

      try aux None 
      with exn -> Util.log "CouchDB: FAIL: %s" (Printexc.to_string exn)
  end 

end

let compile_views () = Views.compile ()

(* A common utility function for generating view urls *)

let view_query_url 
    ~database
    ~design
    ~name
    ~keyfmt
    ?startkey   
    ?startid
    ?endkey
    ?endid
    ?limit
    ?(descending=false)
    ?(include_docs=false)
    ?(endinclusive=true)
    ?(reduce=true)
    ?group_level
    ?(group=false)
    () =

  let key k = Json_io.string_of_json ~recursive:true ~compact:true (keyfmt k) in
  let keep = function (x, None) -> None | (x, Some y) -> Some (x,y) in
  let args = BatList.filter_map keep [
    "include_docs",  (if include_docs then Some "true" else None) ;
    "limit",          (BatOption.map string_of_int limit) ;
    "inclusive_end",  (if not endinclusive then Some "false" else None) ;
    "reduce",         (if not reduce then Some "false" else None) ;
    "group_level",    (BatOption.map string_of_int group_level) ;
    "group",          (if group then Some "true" else None) ;
    "descending",     (if descending then Some "true" else None) ;
    "startkey",       (BatOption.map key startkey) ;
    "endkey",         (BatOption.map key endkey) ;
    "startkey_docid", (BatOption.map Id.str startid) ;
    "endkey_docid",   (BatOption.map Id.str endid) ;    
  ] in
  
  let url = 
    String.concat "" [
      database.ImplDB.db_prefix ;
      String.concat "/" ["_design";design;"_view";name] ;
      "?" ;
      Netencoding.Url.mk_url_encoded_parameters args
    ]
  in
  
  url
    
(* A common utility function for extracting and traversing view results. This
   is actully a monad that retrieves a list of JSON items from the view
   response, so it may be manipulated from then on. 
*)
  
let rec process_view_results ?(retries=5) url =
  try Util.logreq "GET %s" url ;
      let json_str = Http_client.Convenience.http_get url in
      try Run.return begin 
	    Json_io.json_of_string ~recursive:true json_str 
            |> Json_type.Browse.objekt
	    |> List.assoc "rows"
	    |> Json_type.Browse.array	  	  
          end
      with Json_type.Json_error error as exn ->
	Util.log "CouchDB : `%s` : `%s` on: %s" url error json_str ;
	if retries <= 0 then raise exn else process_view_results ~retries:(retries-1) url
  with 
    | Http_client.Http_error (status,desc) as exn ->
      Util.log "CouchDB : `%s` : %d %s`" url status desc ;
      if retries <= 0 then raise exn else process_view_results ~retries:(retries-1) url
    | Http_client.Http_protocol (exn') as exn ->
      Util.log "CouchDB : `%s` : %s" url (Printexc.to_string exn') ;
      if retries <= 0 then raise exn else process_view_results ~retries:(retries-1) url
    
(* Implementing the views *)    

module MapView = functor (Def:ImplTypes.MAP_DEF) -> struct

  type map_key = Def.Key.t
  type map_value = Def.Value.t

  type map_iv = <
    id : Id.t ;
    value : map_value
  > ;;

  type map_kiv = <
    id : Id.t ;
    key : map_key ;
    value : map_value
  >

  let with_reduce = Views.register
    ~database:Def.Design.Database.database
    ~design:Def.Design.name
    ~name:Def.name
    ~map:Def.map
    ~reduce:None

  let view = Def.(
    Design.(Database.database.ImplDB.db_name ^ "/" ^ name) ^ "/" ^ name 
  )
    
  let query
      ?startkey
      ?startid
      ?endkey
      ?endid
      ?limit
      ?descending
      ?endinclusive 
      () = 

    let run url = 

      Run.of_call process_view_results url |> Run.bind begin fun list -> 
	
	let list = list |> List.map begin function item ->
	  let list = Json_type.Browse.objekt item in
	  List.assoc "key"   list |> Def.Key.of_json_safe ,
	  List.assoc "id"    list |> Id.of_json_safe ,
	  List.assoc "value" list |> Def.Value.of_json_safe 
	end |> BatList.filter_map begin function 
	  | (Some k, Some i, Some v) -> Some
	    (object
	      method key   = k
	      method id    = i
	      method value = v
	     end)
	  | _ -> None
	end in

	Run.return list 

      end

    in

    let reduce = if !with_reduce then Some false else None in

    let url = view_query_url
      ~database:Def.Design.Database.database
      ~design:Def.Design.name
      ~name:Def.name
      ~keyfmt:Def.Key.to_json
      ?startkey
      ?startid
      ?endkey
      ?endid
      ?limit
      ?descending
      ?endinclusive
      ?reduce
      ()
    in
    
    run url
      
  let by_key key = 
    Run.map (List.map (fun x -> (x :> map_iv))) (query ~startkey:key ~endkey:key ())

end

module DocView = functor (Def:ImplTypes.DOC_DEF) -> struct

  type doc_key = Def.Key.t
  type doc_value = Def.Value.t
  type doc_doc = Def.Doc.t

  type doc_ivd = <
    id : Id.t ;
    value : doc_value ;
    doc : doc_doc
  >

  type doc_kivd = <
    key : doc_key ;
    id : Id.t ;
    value : doc_value ;
    doc : doc_doc
  >

  let with_reduce = Views.register
    ~database:Def.Design.Database.database
    ~design:Def.Design.name
    ~name:Def.name
    ~map:Def.map
    ~reduce:None

  let view = Def.(Design.(Database.database.ImplDB.db_name ^ "/" ^ name) ^ "/" ^ name) 

  let doc_query
      ?startkey
      ?startid
      ?endkey
      ?endid
      ?limit
      ?descending
      ?endinclusive 
      () = 

    let run url = 

      Run.of_call process_view_results url |> Run.bind begin fun list -> 

	(* Extract the fields of each row *)
	let list = 
	  List.map begin function item ->
	    let list = Json_type.Browse.objekt item in
	    List.assoc "key"   list,
	    List.assoc "id"    list,
	    List.assoc "value" list,
	    List.assoc "doc"   list
	  end list
	in 
	
	(* Add all found documents to the cache *)
	let docs = 
	  BatList.filter_map begin fun (_,i,_,d) ->
	    match Id.of_json_safe i with None -> None | Some i -> 
	      let key = ImplCache.CacheKey.make Def.Design.Database.database i in
	      let doc = ImplCache.cached_of_json d in
	      Some (key, Some doc) 
	  end list
	in

	ImplCache.cache_values docs |> Run.bind begin fun () -> 
	
	  (* Eliminate unparseable documents *)
	  let list = 
	    list |> List.map begin function (k,i,v,d) ->
	      Def.Key.of_json_safe k,
	      Id.of_json_safe i,
	      Def.Value.of_json_safe v,
	      Def.Doc.of_json_safe d
	    end |> BatList.filter_map begin function
	      | Some k, Some i, Some v, Some d -> Some
		(object
		  method key   = k
		  method id    = i
		  method value = v
		  method doc   = d
		 end)
	      | _ -> None
	    end
	  in
	
	  Run.return list
	end
      end
    in

    let reduce = if !with_reduce then Some false else None in

    let url = view_query_url
      ~database:Def.Design.Database.database
      ~design:Def.Design.name
      ~name:Def.name
      ~keyfmt:Def.Key.to_json
      ?startkey
      ?startid
      ?endkey
      ?endid
      ?limit
      ?descending
      ?endinclusive
      ~include_docs:true
      ?reduce
      ()
    in
    
    run url

  let doc key = 
    Run.map (List.map (fun x -> (x :> doc_ivd))) (doc_query ~startkey:key ~endkey:key ())

end

module ReduceView = functor (Def:ImplTypes.REDUCE_DEF) -> struct 

  type reduce_key = Def.Key.t
  type reduce_value = Def.Value.t 

  let _ = Views.register
    ~database:Def.Design.Database.database
    ~design:Def.Design.name
    ~name:Def.name
    ~map:Def.map
    ~reduce:(Some Def.reduce)

  let view = Def.(Design.(Database.database.ImplDB.db_name ^ "/" ^ name) ^ "/" ^ name) 

  let reduce_query
      ?startkey
      ?endkey
      ?limit
      ?endinclusive 
      () = 

    let run url = 

      Run.of_call process_view_results url |> Run.bind begin fun list -> 

	let list = list |> List.map begin function item ->
	  let list = Json_type.Browse.objekt item in
	  List.assoc "key"   list |> Def.Key.of_json_safe ,
	  List.assoc "value" list |> Def.Value.of_json_safe 
	end |> BatList.filter_map begin function 
	  | (Some k, Some v) -> Some (k,v)
	  | _ -> None
	end in

	Run.return list 

      end
    in

    let url = view_query_url
      ~database:Def.Design.Database.database
      ~design:Def.Design.name
      ~name:Def.name
      ~keyfmt:Def.Key.to_json
      ?startkey
      ?endkey
      ?limit
      ?endinclusive
      ?group_level:Def.level
      ~group:Def.group
      ()
    in
    
    run url 

  let reduce key = 
    let keep = function
      | [] -> None
      | (_,v) :: _ -> Some v
    in
    Run.map keep (reduce_query ~startkey:key ~endkey:key ~limit:1 ())
      
end
