(* Ohm is © 2013 Victor Nicollet *)

open BatPervasives

module type RESET = sig

  val send : unit -> unit

  val check_wrapper : ('a -> unit) -> 'a -> unit 

  val resetting : unit -> bool

  val check : unit -> unit

end

module UsingCouchDB : functor (DB : CouchDB.DATABASE) -> RESET
module Make : functor (DB : CouchDB.DATABASE) -> RESET
