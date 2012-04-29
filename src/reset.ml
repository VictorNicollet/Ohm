(* Ohm is © 2011 Victor Nicollet *)

open BatPervasives

module type RESET = sig

  val run : unit -> (#CouchDB.ctx,unit) Run.t

  val check_wrapper : ('a -> unit) -> 'a -> unit 

  val resetting : unit -> bool

  val check : unit -> unit

end

module Make = 
  functor (DB : CouchDB.DATABASE) ->
struct

  let id = Id.of_string "reset"

  module Reset = Fmt.Make(struct
    type json t = <
      t    : string ;
      time : string 
    > 
  end)

  module MyTable = CouchDB.Table(DB)(Id)(Reset)

  let _default = Util.string_of_time (Unix.gettimeofday ()) 

  let _get () = 
    let time = function
      | Some value -> value # time
      | None       -> _default
    in
    MyTable.get id |> Run.map time

  let _initial = Run.eval (new CouchDB.init_ctx) (_get ())

  let run () =
    Run.context |> Run.bind begin fun ctx -> 
      let reset = object 
	method t    = "rset" 
	method time = Util.string_of_time (ctx # time) 
      end in    
      MyTable.transaction id (MyTable.insert reset) 
      |> Run.map (fun _ -> Util.log "Reset.perform : request sent") 
    end

  let resetting () = 
    Run.eval (new CouchDB.init_ctx) (_get () |> Run.map (fun x -> x <> _initial)) 

  let check () = 
    if resetting () then begin 
      Util.log "Reset.check: reset requested at %s, shutting down" _initial ;
      exit 0
    end
       
  let check_wrapper f a = 
    f a ;
    check () 

end
