(* Ohm is © 2012 Victor Nicollet *)

open Universal

type implementation = Id.t ref

class virtual ctx = object
  method virtual couchdb : CouchDB.ctx
  val async = ref (Id.gen ())
  method async = async
end

exception Reschedule

type ('ctx,'a) task = ?delay:float -> 'a -> ('ctx,unit) Run.t

module Make = functor(DB:CouchDB.CONFIG) -> struct

  let stats () = return (object
    method failed = 0
    method pending = 0
    method running = 0
  end) 

  let save_task delay name args = return ()
  let log fmt = Util.log fmt
  let run_task defined ctx = None

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

    method run new_ctx = 
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

      let list = (run_task defined,1,ref None) :: periodic in

      process false list

  end

end
