(* Jogging is © 2011 Victor Nicollet *)

type ('arg,'ctx) template
type ('arg,'ctx) t = ('arg,'ctx) template

val to_html : ('arg,View.Context.box)  t -> 'arg -> I18n.t -> View.Context.box  View.t
val to_text : ('arg,View.Context.text) t -> 'arg -> I18n.t -> View.Context.text View.t

module type JS_HTML_TEMPLATE_DEF = sig
  type t 
  val source : I18n.language -> string
  val script : I18n.language -> (string * (t -> Json_type.t)) list
  val mapping : I18n.language -> (string * ((t, View.Context.box) template -> (t, View.Context.box) template)) list
end

module type HTML_TEMPLATE_DEF = sig
  type t 
  val source : I18n.language -> string
  val mapping : I18n.language -> (string * ((t, View.Context.box) template -> (t, View.Context.box) template)) list
end

module type HTML = sig
  type t 
  val template : I18n.language -> (t, View.Context.box) template
  val render : t -> I18n.t -> View.Context.box -> View.Context.box
end

module type TEXT_TEMPLATE_DEF = sig
  type t 
  val source : I18n.language -> string
  val mapping : I18n.language -> (string * ((t, View.Context.text) template -> (t, View.Context.text) template)) list
end

module type TEXT = sig
  type t 
  val template : I18n.language -> (t, View.Context.text) template
  val render : t -> I18n.t -> View.Context.text -> View.Context.text
end

module type LOADER = sig 

  val load : string 
    -> string
    -> (string * (('arg,(#View.Context.text as 'ctx)) t -> ('arg,'ctx) t)) list 
    -> [< `Text | `Html]
    -> ('arg,'ctx) t 

  val jsload : string 
    -> string
    -> (string * (('arg,View.Context.box) t -> ('arg,View.Context.box) t)) list 
    -> (string * ('arg -> Json_type.t)) list
    -> [< `Text | `Html]
    -> ('arg,View.Context.box) t 

  val clear : unit -> unit

  val save : unit -> unit

  module MakeLoader : 
    functor (From : sig val from : string end) ->
  sig

    module Html   : functor (Def:   HTML_TEMPLATE_DEF) -> HTML with type t = Def.t
    module JsHtml : functor (Def:JS_HTML_TEMPLATE_DEF) -> HTML with type t = Def.t
    module Text   : functor (Def:   TEXT_TEMPLATE_DEF) -> TEXT with type t = Def.t

  end

end


module Loader : functor (Config:CouchDB.DATABASE) -> LOADER

module Mk : sig

  val empty : ('arg,'ctx) t

  val put  : string              -> ('arg, (#View.Context.text as 'ctx)) t -> ('arg, 'ctx) t
  val esc  : ('arg -> string)    -> ('arg, (#View.Context.text as 'ctx)) t -> ('arg, 'ctx) t
  val str  : ('arg -> string)    -> ('arg, (#View.Context.text as 'ctx)) t -> ('arg, 'ctx) t
  val js   : JsCode.t            -> ('arg, View.Context.box) t -> ('arg, View.Context.box) t
  val code : ('arg -> JsCode.t)  -> ('arg, View.Context.box) t -> ('arg, View.Context.box) t
  val text : ('arg -> View.text) -> ('arg, (#View.Context.text as 'ctx)) t -> ('arg, 'ctx) t
  val html : ('arg -> View.html) -> ('arg, View.Context.box) t -> ('arg,View.Context.box) t 

  val int  : ('arg -> int)       -> ('arg, (#View.Context.text as 'ctx)) t -> ('arg, 'ctx) t

  val i18n : I18n.text           -> ('arg, (#View.Context.text as 'ctx)) t -> ('arg, 'ctx) t
  val trad : ('arg -> I18n.text) -> ('arg, (#View.Context.text as 'ctx)) t -> ('arg, 'ctx) t

  val iput  : (I18n.t -> string)            -> ('arg, (#View.Context.text as 'ctx)) t -> ('arg, 'ctx) t
  val iesc  : ('arg -> I18n.t -> string)    -> ('arg, (#View.Context.text as 'ctx)) t -> ('arg, 'ctx) t
  val istr  : ('arg -> I18n.t -> string)    -> ('arg, (#View.Context.text as 'ctx)) t -> ('arg, 'ctx) t
  val ijs   : (I18n.t -> JsCode.t)          -> ('arg, View.Context.box) t -> ('arg, View.Context.box) t
  val icode : ('arg -> I18n.t -> JsCode.t)  -> ('arg, View.Context.box) t -> ('arg, View.Context.box) t
  val itext : ('arg -> I18n.t -> View.text) -> ('arg, (#View.Context.text as 'ctx)) t -> ('arg, 'ctx) t
  val ihtml : ('arg -> I18n.t -> View.html) -> ('arg, View.Context.box) t -> ('arg,View.Context.box) t 

  val verbatim : (View.html,View.Context.box) t

  val sub     : ('arg -> 'arg2)        -> ('arg2, 'ctx) t -> ('arg, 'ctx) t -> ('arg, 'ctx) t
  val list    : ('arg -> 'arg2 list)   -> ('arg2, 'ctx) t -> ('arg, 'ctx) t -> ('arg, 'ctx) t
  val list_or : ('arg -> 'arg2 list)   -> ('arg2, 'ctx) t -> ('arg, 'ctx) t -> ('arg, 'ctx) t -> ('arg, 'ctx) t
  val sub_or  : ('arg -> 'arg2 option) -> ('arg2, 'ctx) t -> ('arg, 'ctx) t -> ('arg, 'ctx) t -> ('arg, 'ctx) t
    
end
