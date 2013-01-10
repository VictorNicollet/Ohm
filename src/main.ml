(* Ohm is © 2012 Victor Nicollet *)

module Make = 
  functor (Reset:Reset.RESET) ->
struct

  let run ?async role = 

    let web_loop () = 
      if not (Reset.resetting ()) then 
	Action.run (Reset.check_wrapper Action.dispatch)
    in

    let bot_loop () = 
      let last_compact = ref 0. in
      if not (Reset.resetting ()) then 
	let rec loop_check () = 

	  (* Stop looping if a reset is requested. *)
	  Reset.check () ;

	  (* Compact databases every hour *)
	  let now = Unix.gettimeofday () in
	  if now > !last_compact +. 3600.0 then 
	    ( last_compact := now ; CouchDB.compact ()) ;

	  (* Perform one run of async processing. *)
	  let wait = 
	    match async with None -> true | Some async -> 
	      not (async ())
	  in

	  (* If the run finished all tasks, sleep for one second. *)
	  if wait then Unix.sleep 1 ;
	      
	  loop_check () ;

	in 
	loop_check () 
    in
      	
    match role with
      | `Web   -> web_loop ()
      | `Bot   -> bot_loop ()
      | `Put   -> CouchDB.compile_views ()
      | `Reset -> Reset.send ()

end

