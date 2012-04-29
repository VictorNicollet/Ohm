(* Jogging is © 2011 Victor Nicollet *)
type t
type language = [ `Fr ]

type html = t -> View.html

val languages : language list

module Source : JoyA.FMT with type t = (string * string) list

module Text : Fmt.FMT with type t = [ `text of string | `label of string ]

type text = Text.t 

val source : t -> Source.t
val empty : language -> t
val get : t -> text -> #View.Context.text View.t
val translate : t -> text -> string
val label : string -> t -> #View.Context.text View.t
val get_param : t -> string -> (#View.Context.text as 'ctx) View.t list -> 'ctx View.t
val language : t -> language

module Loader : 
  functor (Config:CouchDB.DATABASE) ->
sig

  val load : Id.t -> language -> t 
  val save : Id.t -> ?volatile:bool -> ?parent:Id.t -> Source.t -> (#CouchDB.ctx,unit) Run.t
  val copy : src:Id.t -> dest:Id.t -> (#CouchDB.ctx,unit) Run.t

end
