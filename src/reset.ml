(* Ohm is © 2013 Victor Nicollet *)

open BatPervasives

module type RESET = sig

  val send : unit -> unit

  val check_wrapper : ('a -> unit) -> 'a -> unit 

  val resetting : unit -> bool

  val check : unit -> unit

end

module UsingCouchDB = 
  functor (DB : CouchDB.DATABASE) ->
struct

  let id = Id.of_string "reset"

  module Reset = Fmt.Make(struct
    type json t = <
      t    : string ;
      time : string 
    > 
  end)

  module Tbl = CouchDB.Table(DB)(Id)(Reset)

  let _default = Util.string_of_time (Unix.gettimeofday ()) 

  let _get () = 
    Run.map (BatOption.default _default) (Tbl.using id (#time)) 

  let _initial = Run.eval (new CouchDB.init_ctx) (_get ())

  let send () =
    Run.eval (new CouchDB.init_ctx) begin 
      Run.context |> Run.bind begin fun ctx -> 
	let reset = object 
	  method t    = "rset" 
	  method time = Util.string_of_time (ctx # time) 
	end in    
	Tbl.set id reset 
	|> Run.map (fun _ -> Util.log "Reset.perform : request sent") 
      end
    end 

  let resetting () = 
    Run.eval (new CouchDB.init_ctx) (_get () |> Run.map ((<>) _initial)) 

  let check () = 
    if resetting () then begin 
      Util.log "Reset.check: reset requested at %s, shutting down" _initial ;
      exit 0
    end
       
  let check_wrapper f a = 
    try f a ; check () with exn -> check () ; raise exn

end

module Make = UsingCouchDB
