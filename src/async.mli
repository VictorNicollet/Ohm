(* Ohm is © 2012 Victor Nicollet *)

(** The {b Async} layer: delegating tasks to an asynchronous bot.

    @author Victor Nicollet
    @version 0.9
*)

(** {2 Dealing with the context} *)

(** An abstract implementation type. You do not need this, manipulate 
    {!ctx} instances instead. 
*)
type implementation 

(** An asynchronous execution context. Expects a member function that returns a
    CouchDB context, since that context is used to save and load everything. 
*)
class virtual ctx : object
  method virtual couchDB : CouchDB.implementation
  method virtual time    : float
  method async : implementation
end

(** {2 Implementing and running tasks} *)

(** An exception. Raise this exception from within an asynchronous task to
    cause it to be re-scheduled for a later time. It will not count as 
    having been executed. 
*)
exception Reschedule

(** The type of a task that expects a parameter to be run, represented
    as a plain old function. When called, this function writes the 
    task to the task database, where it will be popped as soon as 
    possible by the asynchronous process. 
    @param delay Delay the execution of the task by this many seconds. 
*)
type ('ctx,'a) task = ?delay:float -> 'a -> ('ctx,unit) Run.t

(** How long does the execution process lock a task for? This is by 
    definition the number of seconds between the first and second
    attempt to execute a task. There is no third attempt. This is
    currently 10 minutes. 
*)
val delay : float

(** When the asynchronous runner runs out of tasks to perform, it sleeps
    for this duration, in seconds. This is currently 2 seconds.
*)
val sleep : float

(** The interface used by task managers. Implemented by the manager 
    provided in functor {!Make} 
*)
class type ['ctx] manager = object
  method define : 'a. string -> 'a Fmt.fmt -> ('a -> ('ctx,unit) Run.t) -> ('ctx,'a) task
  method declare : 'a. string -> 'a Fmt.fmt -> ('ctx,'a) task * (('a -> ('ctx,unit) Run.t) -> unit)
  method periodic : int -> ('ctx,float option) Run.t -> unit
end

(** A task execution environment, using a database to save data. *)
module Make : functor(DB:CouchDB.CONFIG) -> sig

  class ['ctx] manager : object
    constraint 'ctx = #ctx

    (** Define a task by providing a name (used to map the database-serialized
	tasks onto actual functions), a JSON formatter (for serializing the
	arguments to the database) and a body. 
    *)
    method define : 'a. string -> 'a Fmt.fmt -> ('a -> ('ctx,unit) Run.t) -> ('ctx,'a) task

    (** Declare a task. This returns the task and a function that must be 
	called to define the task. *)
    method declare : 'a. string -> 'a Fmt.fmt -> ('ctx,'a) task * (('a -> ('ctx,unit) Run.t) -> unit)

    (** A periodic operation. The function returns the minimum time that should 
	elapse before the function must be called again, in seconds. The integer
	parameter is the priority level: a function with priority 6 will be
	executed six times as often as a function with priority 1 (assuming it
	does not return a non-zero wait time). 
    *)
    method periodic : int -> ('ctx,float option) Run.t -> unit

    (** Statistics about the current state of the environment. *)
    method stats : ('ctx,<
      running : int ;
      pending : int ;
      failed  : int
    >) Run.t 
      
    (** Run tasks (acting as a task execution environment), then 
	return. A function that generates brand new contexts is provided as
	an argument. Returns [true] if there were still tasks to be run, 
	[false] if there is nothing left to run (this information can be
	used to sleep for a little while). 
    *)
    method run : (unit -> 'ctx) -> bool

  end

end

(** Convenience functions *)
module Convenience : sig

  val foreach :
        'ctx # manager
    ->  string
    ->  'key Fmt.fmt
    -> ('key option -> ('ctx, 'key list * 'key option) Run.t)  
    -> ('key -> ('ctx,unit) Run.t) 
    -> ('ctx,unit) task

end
