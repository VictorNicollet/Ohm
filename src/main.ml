(* Ohm is © 2012 Victor Nicollet *)

module Make = 
  functor (Reset:Reset.RESET) ->
struct

  let run () = 

    let web_loop () = 
      if not (Reset.resetting ()) then 
	Action.run (Reset.check_wrapper Action.dispatch)
    in

    let bot_loop () = 
      let last_compact = ref 0. in
      if not (Reset.resetting ()) then 
	let rec loop_check () = 
	  Reset.check () ;
	  let now = Unix.gettimeofday () in
	  if now > !last_compact +. 3600.0 then 
	    ( last_compact := now ; CouchDB.compact ()) ;
	  Unix.sleep 2000 ;
	  loop_check () ;
	in 
	loop_check () 
    in
      	
    match Util.role with
      | `Web   -> web_loop ()
      | `Bot   -> bot_loop ()
      | `Put   -> CouchDB.compile_views ()
      | `Reset -> Run.eval (new CouchDB.init_ctx) (Reset.run ())

end
