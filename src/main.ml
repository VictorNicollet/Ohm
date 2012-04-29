module Make =
  functor (Action:Action.CUSTOMIZABLE) -> 
    functor (Reset:Reset.RESET) -> 
      functor (Template:Template.LOADER) -> 
	functor (Task:Task.TASK) ->
struct

  let run () = 
    
    let web_loop () = 
      Template.clear () ;
      if not (Reset.resetting ()) then 
	Action.run (Reset.check_wrapper Action.dispatch)
    in

    let bot_loop () = 
      Template.clear () ;
      let last_compact = ref 0. in
      if not (Reset.resetting ()) then 
	let loop_check () = 
	  Reset.check () ;
	  let now = Unix.gettimeofday () in
	  if now > !last_compact +. 3600.0 then 
	    ( last_compact := now ; CouchDB.compact ()) ;
	in 
	Task.loop loop_check
    in
      	
    match Util.role with
      | `Web   -> web_loop ()
      | `Bot   -> bot_loop ()
      | `Put   -> CouchDB.compile_views () ; Template.save () 
      | `Reset -> Run.eval (new CouchDB.init_ctx) (Reset.run ())

end
