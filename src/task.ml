(* Ohm is © 2011 Victor Nicollet *)

open Util 
open Json_type
open BatPervasives
open Universal

exception Error of string

type 'arg result = 
  | Finished of 'arg 
  | Failed 
  | Initial  of 'arg
  | Waiting  of 'arg
  | Partial  of 'arg * int * int

type 'arg token = 'arg Fmt.t * Id.t

type 'arg implementation = 'arg -> 'arg token -> (CouchDB.ctx,'arg result) Run.t

type 'arg action = 'arg Fmt.t * string

let _actions = Hashtbl.create 100

let register name fmt impl = 
  let json_impl : Json_type.t implementation = fun json (_,tok) -> 
    match fmt.Fmt.of_json json with
      | None -> 
	log "Action.process : action %s : argument unserialize error" name ;
	Run.return Failed
      | Some arg -> 
	impl arg (fmt,tok) |> Run.map begin function
	  | Finished result -> Finished (fmt.Fmt.to_json result)
	  | Failed          -> Failed
	  | Waiting data    -> Waiting (fmt.Fmt.to_json data)
	  | Initial data    -> Initial (fmt.Fmt.to_json data)
	  | Partial (data, did, todo) -> Partial (fmt.Fmt.to_json data, did, todo)
	end
  in 

  begin 
    try 
      let _ = Hashtbl.find _actions name in  
      log "Action.register : ERROR : duplicate action %s" name ;
    with Not_found -> () 
  end;

  Hashtbl.add _actions name json_impl ;

  (fmt, name)

let declare name fmt = 
  (fmt,name) 

let define (fmt,name) impl = 
  ignore (register name fmt impl)

let to_id (_, id)    = id
let of_id fmt id = (fmt,id)

module Background = struct

  let todo = ref []

  let register count f = 
    todo := (count,f) :: !todo

  let process () = 

    let continue f = try Run.eval (new CouchDB.init_ctx) f with _ -> false in

    let rec aux = function 
      | []         -> false
      | (0,_) :: t -> aux t
      | (n,f) :: t -> let continue = continue f in
		      let result   = aux (((if continue then n - 1 else 0), f) :: t) in
		      continue || result
    in

    aux !todo

end

module type TASK = sig
  val call    : 'arg action -> 'arg -> (#CouchDB.ctx,'arg token) Run.t
  val delay   : float -> 'arg action -> 'arg -> (#CouchDB.ctx,'arg token) Run.t
  val prepare : 'arg action -> 'arg -> (#CouchDB.ctx,'arg token) Run.t
  val start   : 'arg token -> (#CouchDB.ctx, unit) Run.t
  val status  : 'arg token -> (#CouchDB.ctx,'arg result) Run.t
  val process : unit -> bool
  val loop    : (unit -> unit) -> 'a
end

module Make =
  functor (DB:CouchDB.DATABASE) ->
struct

  let delay delay (fmt,name) argument = 
    let json = fmt.Fmt.to_json argument in 
    let id   = Id.gen () in
    let task = Json_type.Build.objekt [
      "t"    , Json_type.Build.string "task" ;
      "time" , Fmt.Float.to_json (delay +. Unix.gettimeofday ()) ;
      "what" , json ;
      "code" , Json_type.Build.string name ;
      "sta"  , Json_type.Build.string "init" ;
    ] in
    
    DB.put id task |> Run.map (const (fmt, id)) 

  let call (fmt,name) argument = 
    delay 0.0 (fmt,name) argument

  let prepare (fmt,name) argument = 
    let json = fmt.Fmt.to_json argument in 
    let id   = Id.gen () in
    let task = Json_type.Build.objekt [
      "t"    , Json_type.Build.string "task" ;
      "time" , Fmt.Float.to_json (Unix.gettimeofday ()) ;
      "what" , json ;
      "code" , Json_type.Build.string name ;
      "sta"  , Json_type.Build.string "wait" ;
    ] in
    
    DB.put id task |> Run.map (const (fmt, id))

  let start (fmt,id) = 

    let start json = 
      try 
	let list   = Json_type.Browse.objekt json in 
	let status = Json_type.Browse.string (List.assoc "sta" list) in 
	if status = "wait" then 
	  let list = ( "sta" , Json_type.Build.string "init" )
	    :: ( BatList.remove_assoc "sta" list ) in
	  (), `put (Json_type.Build.objekt list)
	else
	  (), `keep
      with _ -> 
	(), `keep
    in    

    DB.transaction id (DB.if_exists start) |> Run.map ignore
	
  let status (fmt,id)  = 

    let _status json = 
      let list    = Json_type.Browse.objekt json in
      let status  = Json_type.Browse.string (List.assoc "sta" list) in 
      let what f  = List.assoc "what" list |> fmt.Fmt.of_json |> BatOption.map f in
      match status with 
	| "fail" -> Some Failed
	| "init" -> what (fun d -> Initial d) 
	| "wait" -> what (fun d -> Waiting d) 
	| "fini" -> what (fun d -> Finished d)
	| "part" ->
	  let did  = Json_type.Browse.int (List.assoc "did"  list) in 
	  let todo = Json_type.Browse.int (List.assoc "todo" list) in
	  what (fun d -> Partial (d, did, todo))
	| other -> 
	  log "Task.status : unknown status %s for task %s" 
	    other (Id.str id) ;
	  Some Failed
    in

    DB.get id |> Run.map begin function
      | None -> 
	log "Task.status : looking for unknown task %s" (Id.str id) ;
	Failed
      | Some json -> 
	try 
	  match _status json with
	    | Some value -> value
	    | None -> 
	      log "Task.status : problem parsing result %s" (logjson json) ;
	      Failed
	with 
	  | Json_type.Json_error error ->
	    log "Task.status : %s when reading task %s" error (logjson json) ;
	    Failed 
	  | Not_found -> 
	    log "Task.status : missing field when reading task %s" (logjson json) ;
	    Failed
    end

  module Design = struct
    module Database = DB
    let name = "task"
  end

  module OldView = CouchDB.DocView(struct
    module Key   = Fmt.Float
    module Value = Fmt.Unit
    module Doc   = Fmt.Json

    module Design = Design 

    let name = "old"

    let map = "if ( doc.t   == 'task'
               &&   doc.sta == 'fini') emit(doc.time,null)"
  end)

  module NextView = CouchDB.DocView(struct
    module Key   = Fmt.Float
    module Value = Fmt.Unit
    module Doc   = Fmt.Json

    module Design = Design 

    let name = "next"

    let map = "if ( doc.t   == 'task'
               &&   doc.sta != 'wait'
               &&   doc.sta != 'fini'
               &&   doc.sta != 'fail') emit(doc.time,null)"
  end)

  (* This is an operation that grabs the next available task (name, identifier and
     payload) if present. *)

  let _next : (CouchDB.ctx,(string * Id.t * Json_type.t) option) Run.t = 
    
    (* Lock and return the next task if it's acceptable (its time should be in
       the future, and it should be ready to run).
    *)
    let lock id = 	
      let now = Unix.gettimeofday () in 
      let lock json = 
	let list = Browse.objekt json in
	let time = Fmt.Float.of_json (List.assoc "time" list) in
	if time > now then None, `keep else
	  let what = try Some (List.assoc "what" list) with Not_found -> None in
	  match what with 
	    | Some what -> let name = Browse.string (List.assoc "code" list) in 
			   let tries = 
			     try Browse.int (List.assoc "tries" list)
			     with _ -> 0
			   in 
			   
			   let status = Browse.string (List.assoc "sta" list) in 
			   
			   if status = "fail" || status = "fini" || status = "wait" 
			   then None, `keep 
			   else
			     
			     let expire = now +. 2.0 +. 10.0 *. (float_of_int tries) in
			     
			     let list = list
                               |> BatList.remove_assoc "time"
                               |> BatList.remove_assoc "tries" 
			     in
			     
			     let list = 
			       ("time", Fmt.Float.to_json expire) 
			       :: ("tries", Build.int (tries + 1))
			       :: list
			     in
			     
			     Some (name, id, what), `put (Build.objekt list)
			       
	    | None -> let () = log "Task.process : %s has no 'what' field : giving up" (Id.str id) in
		      let list = ("sta",Build.string "fail") :: BatList.remove_assoc "sta" list in 
		      None, `put (Build.objekt list) 
      in
      
      DB.transaction id (DB.if_exists lock) |> Run.map (BatOption.default None)
    in
          
    (* Identify the next task, grab its id, and fetch it. *)
    NextView.doc_query ~startkey:0.0 ~limit:1 () 
    |> Run.bind (Util.first
		    |- BatOption.map (#id |- lock)
		    |- BatOption.default (Run.return None))                 

  (* The prcoessing function itself, grabs a new task, initializes a database context, and 
     performs the task. 
  *)

  let process () = 

    (* This function writes the task back. To be used as part of a transaction. *)
    let _writeback_task id result task =
      try 
	let list = 
	  Json_type.Browse.objekt task  
	  |> BatList.remove_assoc "did"  
	  |> BatList.remove_assoc "todo"  
	  |> BatList.remove_assoc "what" 
	  |> BatList.remove_assoc "sta" 
	  |> BatList.remove_assoc "time"
	in
	let list = ("time", Fmt.Float.to_json (Unix.gettimeofday ())) :: list in 
	let list = 
	  match result with 
	    | Failed ->
	      ("sta", Json_type.Build.string "fail") :: list
	    | Finished arg ->
	      ("sta", Json_type.Build.string "fini") 
	      :: ("what", arg) :: list
	    | Initial arg ->
	      ("sta", Json_type.Build.string "init")
	      :: ("what", arg) :: list
	    | Waiting arg ->
	      ("sta", Json_type.Build.string "wait")
	      :: ("what", arg) :: list
	    | Partial (arg, did, todo) ->
	      ("sta", Json_type.Build.string "part")
	      :: ("what", arg) 
	      :: ("did", Json_type.Build.int did)
	      :: ("todo", Json_type.Build.int todo)
	      :: list
	in
	true, `put (Json_type.Build.objekt list)
      with exn ->
	log "Task.process : %s on task %s : %s" 
	  (Printexc.to_string exn) (Id.str id) (logjson task) ;
	false, `keep
    in

    (* The actual writeback operation. *)
    let _writeback id result = 
      DB.transaction id (DB.if_exists (_writeback_task id result))
      |> Run.map (BatOption.default false)
    in

    let _process name id what = 
      try 
	log "Task.process : %s : %s ( %s )" (Id.str id) name (logjson what) ;	
	let impl = Hashtbl.find _actions name in
	impl what (Fmt.Json.fmt,id) |> Run.bind (_writeback id)
      with Not_found -> 
	Util.log "Task.process : %s : action %s not defined" (Id.str id) name ;
	Run.return false	  
    in
    
    let _fetch_and_process = 
      _next |> Run.bind begin function
	| None                -> Run.return false
	| Some (name,id,task) -> _process name id task
      end
    in

    try Run.eval (new CouchDB.init_ctx) _fetch_and_process with exn -> 
      Util.log "Task.process : failed with exception %s" (Printexc.to_string exn) ; 
      false

  let cleanup () = 
    let _cleanup = 
      Run.context |> Run.bind begin fun ctx ->
	let now = ctx # time in 
	let lifetime = 3600. in
	OldView.doc_query ~startkey:0.0 ~endkey:(now -. lifetime) ~limit:1 ()
	|> Run.map Util.first 
	|> Run.bind begin function 
	    | None -> Run.return ()
	    | Some item ->
	      Util.log "Cleaning up task %s" (Id.str item # id) ;
	      DB.transaction (item # id) (DB.remove) |> Run.map ignore
	end
      end
    in
    Run.eval (new CouchDB.init_ctx) (_cleanup)

  let rec loop prepare = 
    
    prepare () ;

    let ran =
      try process ()
      with exn -> 
	log "Task.process : failed with exception %s"
	  (Printexc.to_string exn) ; 
	false
    in
    if not ran then begin
      let ran = Background.process () in
      if not ran then begin
	cleanup () ;
	log "Nothing to do..." ;
	Unix.sleep 2 ;
      end
    end ;

    loop prepare

end
