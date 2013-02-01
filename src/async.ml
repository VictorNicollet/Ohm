(* Ohm is © 2012 Victor Nicollet *)

open Universal

type implementation = unit

class virtual ctx = object
  method virtual couchDB : CouchDB.implementation
  method virtual time    : float
  method async = ()
end

let delay = 600. 
let sleep = 2.

exception Reschedule

type ('ctx,'a) task = ?delay:float -> 'a -> ('ctx,unit) Run.t

class type ['ctx] manager = object
  method define : 'a. string -> 'a Fmt.fmt -> ('a -> ('ctx,unit) Run.t) -> ('ctx,'a) task
  method declare : 'a. string -> 'a Fmt.fmt -> ('ctx,'a) task * (('a -> ('ctx,unit) Run.t) -> unit)
  method periodic : int -> ('ctx,float option) Run.t -> unit
end

module Make = functor(DB:CouchDB.CONFIG) -> struct

  let log fmt = Util.log fmt

  module Task = struct
    module T = struct
      type json t = {
	time    "t" : float ;
	name    "n" : string ;
	calls   "c" : int ;
	args    "a" : Json.t 
      }
    end
    include T
    include Fmt.Extend(T)
  end

  let lock until t = 
    Task.({ t with time = until ; calls = succ t.calls })

  let unlock t = 
    Task.({ t with calls = pred t.calls })

  (* These functions implement the task manipulation at the database level. 
     They depend on CouchDB for doing the work. *)

  module MyDB = CouchDB.Database(DB)
  module Tbl = CouchDB.Table(MyDB)(Id)(Task)
  module Design = struct
    module Database = MyDB
    let name = "async"
  end

  module StatsView = CouchDB.ReduceView(struct
    module Key    = Fmt.Unit
    module Value  = Fmt.Make(struct type json t = int * int * int end)
    module Design = Design 
    let name = "stats"
    let map  = "var c = doc.c, a = [0,0,0]; 
                if (c > 2) c = 2;
                a[c] = 1;
                emit(null,a);"
    let reduce = "var a = [0,0,0];
                  for (var i = 0; i < values.length; ++i) {
                    a[0] += values[i][0];
                    a[1] += values[i][1];
                    a[2] += values[i][2];
                  } 
                  return a;"
    let group = false
    let level = None
  end)

  let stats () = 
    let! stats = ohm $ StatsView.reduce () in
    let  _0, _1, _2 = BatOption.default (0,0,0) stats in
    return (object
      method pending = _0
      method running = _1
      method failed  = _2
    end) 

  let save_task delay name args = 
    let! time = ohmctx (#time) in
    let  time = match delay with None -> time | Some delay -> time +. delay in 
    let! _    = ohm $ Tbl.create Task.({ time ; calls = 0 ; name ; args }) in
    return ()

  module TaskView = CouchDB.DocView(struct
    module Key    = Fmt.Float
    module Value  = Fmt.Unit
    module Doc    = Task
    module Design = Design
    let name = "next"
    let map  = "if (doc.c < 2) emit(doc.t);"
  end)

  let rec find_next retries =

    if retries = 0 then return None else

      let! now = ohmctx (#time) in
      
      let! doc = ohm_req_or (return None) $ TaskView.doc_query_first 
	~startkey:0.0
	~endkey:now
	()
      in
      
      let id, task = doc # id, doc # doc in
      let unlock   = now +. delay in
      let task     = lock unlock task in
      
      let! result  = ohm $ Tbl.Raw.put id task in 
      match result with 
	| `collision -> find_next (retries-1)
	| `ok        -> return $ Some (id,task)

  let run_next defined reschedule = 
    
    let! id, task = ohm_req_or (return (Some sleep)) $ find_next 5 in 

    let  () = log "Ohm.Async: run task %s : %S" (Id.str id) task.Task.name in

    (* Specify a reschedule operation in case we're interrupted by [raise Reschedule] *)
    let  () = reschedule := (
      let! _ = ohm $ Tbl.set id (unlock task) in
      return (log "Ohm.Async: reschedule %s" (Id.str id))
    )  in

    let call = try BatPMap.find task.Task.name defined with Not_found -> 
      log "Ohm.Async: task %S does not exist" task.Task.name ;
      ( fun json -> return () ) 
    in
    
    let! () = ohm $ call task.Task.args in    
    let! () = ohm $ Tbl.delete id in 
    
    return None

  let run_task ?timeout defined ctx = 
    let reschedule = ref (return ()) in
    let timeout = BatOption.map Run.timeout timeout in
    try Run.eval ?timeout ctx (run_next defined reschedule) 
    with 
      | Reschedule -> Run.eval ctx (!reschedule) ; None
      | Run.Timeout -> Util.log "Ohm.Async: task timed out" ; None
      | exn -> Util.log "Ohm.Async: task failed with %S" (Printexc.to_string exn) ; None

  (* This is the manager. It has no dependency on CouchDB. *)

  class ['ctx] manager = object (self)
    constraint 'ctx = #ctx 

    val mutable defined   = BatPMap.empty
    val mutable undefined = BatPSet.empty 
    val mutable periodic  = []

    method define : 'a. string -> 'a Fmt.fmt -> ('a -> ('ctx,unit) Run.t) -> ('ctx,'a) task = 
      fun name format body -> 
	let task, define = self # declare name format in
	define body ; task
	
    method declare : 'a. string -> 'a Fmt.fmt -> ('ctx,'a) task * (('a -> ('ctx,unit) Run.t) -> unit) = 
      fun name format -> 
	undefined <- BatPSet.add name undefined ;
	let task ?delay args = save_task delay name (format.Fmt.to_json args) in
	let define body =
	  let call json = 
	    match format.Fmt.of_json json with 
	      | None      -> return (log "Ohm.Async: could not parse arguments for %S" name)
	      | Some args -> body args
	  in
	  if BatPMap.mem name defined then 
	    log "Ohm.Async: task %S defined twice" name ; 
	  undefined <- BatPSet.remove name undefined ;
	  defined   <- BatPMap.add name call defined 
	in
	task, define
      
    method periodic : int -> ('ctx,float option) Run.t -> unit = 
      fun count f -> 
	let f ctx = 
	  try Run.eval ctx f
	  with exn -> 
	    log "Ohm.Async: periodic raised %S" (Printexc.to_string exn) ;
	    Some 1.
	in
	if count > 0 then 
	  periodic <- (f, count, ref None) :: periodic 

    method stats : ('ctx,'stats) Run.t = stats ()

    method run ?timeout new_ctx = 
      if not (BatPSet.is_empty undefined) then begin
	BatPSet.iter (log "Ohm.Async: task %S not defined") undefined ;
	failwith "Async tasks were not defined"
      end ;

      let rec process more = function 
	| [] -> more
	| (_,0,_) :: tail -> process true tail
	| (f,n,t) :: tail -> 
	  let time = Unix.gettimeofday () in
	  match !t with Some t when t > time -> process more tail | _ -> 
	    t := f (new_ctx ()) ;
	    let next = if !t = None then (f,n-1,t) :: tail else tail in
	    process more next 
      in

      let list = (run_task ?timeout defined,1,ref None) :: periodic in

      process false list

  end

end

module Convenience = struct

  let foreach (manager:'c # manager) name fmt iterator action = 
    let task, define = manager # declare name (Fmt.optional fmt) in
    let () = define begin fun key -> 
      let! list, next = ohm $ iterator key in
      let! () = ohm $ Run.list_iter action list in
      if next = None then return () else task next
    end in 
    fun ?delay () -> task ?delay None

end
