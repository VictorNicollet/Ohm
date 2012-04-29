(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives

type t = {
  db_id     : int ;
  db_host   : string ;
  db_port   : int ;
  db_name   : string ;
  db_prefix : string
}

let databases = ref []

let make_database = 
  let id   = ref 0 in
  let memo = Util.memoize begin fun (host,port,database) ->

    incr id ;

    (* Compute the prefix once and for all *)
    let prefix = 
      Neturl.({ null_url_syntax with 
	url_enable_scheme = Url_part_required ;
	url_enable_host   = Url_part_required ;
	url_enable_port   = Url_part_allowed ;
	url_enable_path   = Url_part_required }) 
      |> Neturl.make_url 
	  ~scheme:"http"
	  ~host
	  ~port
	  ~path:["";database]	
      |> Neturl.string_of_url
    in

    (* Construct the database object *)
    let db = { db_id     = !id ;
	       db_host   = host ;
	       db_port   = port ;
	       db_name   = database ;
	       db_prefix = prefix ^ "/" }
    in

    (* Register it for compaction *)
    databases := db :: !databases ;

    (* Create the database if it does not exist yet. *)
    let () = 
      try 
	Util.logreq "GET %s" db.db_prefix ;
	let _ = Http_client.Convenience.http_get db.db_prefix in ()
      with
	|  Http_client.Http_error (status,_) ->
	  if status = 404 then begin
	    Util.log "CouchDB: %s:%d/%s does not exist, creating" 
	      db.db_host db.db_port db.db_name ;
	    Util.logreq "PUT %s" db.db_prefix ;
	    ignore (Http_client.Convenience.http_put db.db_prefix "")
	  end else
	    Util.log "CouchDB: %s:%d/%s : HTTP status : %d" 
	      db.db_host db.db_port db.db_name status	      	
	|  Http_client.Http_protocol error ->
	    Util.log "CouchDB: %s:%d/%s : HTTP error : %s" 
	      db.db_host db.db_port db.db_name (Printexc.to_string error)	
    in
    
    db
    
  end in
  fun ~host ~port ~database -> memo (host,port,database)

(* Compaction: traverse all registered databased and post to _compact *)
    
let compact () = 
  !databases |> List.iter begin fun db ->
    let url = db.db_prefix ^ "_compact" in    
    Util.logreq "POST %s" url ;
    let call = new Http_client.post_raw url "" in
    (call # request_header `Base) # update_field "Content-Type" "application/json" ;
    let pipeline = new Http_client.pipeline in
    pipeline # add call ;
    pipeline # run () ;
    Util.log "CouchDB: compact: %s" db.db_name ;
  end
