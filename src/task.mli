(* Ohm is © 2011 Victor Nicollet *)

(** The {b Async} layer: primitives for delaying the execution of code. 

    Code is grouped as a function which expects an argument of a pre-determined type, and returns 
    a {!type:Task.result} to indicate the new state of the task (finished, failed...). The function
    may perform any necessary operations, but should attempt to only perform invariant operations
    in order to survive a crash or exception - should an error happen while a task was being executed,
    the execution will start from the beginning. 

    Tasks are stored in a CouchDB instance by the code that calls them, and read from that same
    database by the code that runs them - usually a different process dedicated only
    to running tasks. 

    @author Victor Nicollet
    @version 1.0
*)

(** A temporary error has happened while running delayed code. 

    This exception is never raised by the {!Task} module: you are expected to raise it yourself
    from your own delayed code to signal that a temporary error has happened. This causes the error message
    to be logged, and the delayed code to be scheduled for ulterior re-execution. 

    You should not use this for permanent errors, since it will cause the delayed code to be re-executed
    until it works and thus clog the asynchronous pipeline unnecessarily. Instead, when a permanent
    error happens, have the delayed code return a {!Task.result} of [Task.Failed].
*)
exception Error of string


type 'arg result = 
  | Finished of 'arg 
  | Failed 
  | Initial of 'arg
  | Waiting of 'arg
  | Partial of 'arg * int * int

type 'arg token 

type 'arg implementation = 'arg -> 'arg token -> (CouchDB.ctx,'arg result) Run.t

type 'arg action 

val register : string -> 'arg Fmt.t -> 'arg implementation -> 'arg action

val declare : string -> 'arg Fmt.t -> 'arg action
val define  : 'arg action -> 'arg implementation -> unit

val to_id : 'arg token -> Id.t
val of_id : 'arg Fmt.t -> Id.t -> 'arg token 

module Background : sig

  val register : int -> (CouchDB.ctx,bool) Run.t -> unit

end

module type TASK = sig

  val call    : 'arg action -> 'arg -> (#CouchDB.ctx,'arg token) Run.t

  val delay   : float -> 'arg action -> 'arg -> (#CouchDB.ctx,'arg token) Run.t

  val prepare : 'arg action -> 'arg -> (#CouchDB.ctx,'arg token) Run.t 

  val start   : 'arg token -> (#CouchDB.ctx,unit) Run.t

  val status  : 'arg token -> (#CouchDB.ctx,'arg result) Run.t

  val process : unit -> bool

  val loop    : (unit -> unit) -> 'a

end

module Make :
  functor (Db:CouchDB.DATABASE) -> TASK
