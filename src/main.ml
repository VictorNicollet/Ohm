(* Ohm is © 2013 Victor Nicollet *)

module Make = 
  functor (Reset:Reset.RESET) ->
struct

  let run ?async role = 

    let web_loop () = 
      if not (Reset.resetting ()) then       	
	Action.run (Reset.check_wrapper Action.dispatch)
    in

    let bot_loop () = 
      if not (Reset.resetting ()) then 
	let rec loop_check () = 

	  (* Stop looping if a reset is requested. *)
	  Reset.check () ;

	  (* Compact databases every hour *)
	  Sig.Std.Bot.tick_ () ; 

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
      | `Web   -> Sig.Std.Web.init_ () ; web_loop ()
      | `Bot   -> Sig.Std.Bot.init_ () ; bot_loop ()
      | `Put   -> Sig.Std.Put.once_ () 
      | `Reset -> Reset.send ()

end

