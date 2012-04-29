(* Ohm is © 2011 Victor Nicollet *)

type ('cfg,'ctx) field = {
  name   : string ;
  render : Id.t -> I18n.t -> 'cfg -> 'ctx View.t ;
  label  : 'cfg -> I18n.text ;
  json   : 'cfg -> bool
}

val field : 
     ?json:('cfg -> bool)
  ->  name:string 
  ->  render:(Id.t -> I18n.t -> 'cfg -> 'ctx View.t) 
  ->  label:('cfg -> I18n.text)
  ->  unit
  ->  ('cfg,'ctx) field

val text     : name:string -> label:string -> ('cfg,View.Context.box) field
val password : name:string -> label:string -> ('cfg,View.Context.box) field
val checkbox : name:string -> label:string -> ('cfg,View.Context.box) field
val textarea : name:string -> label:string -> ('cfg,View.Context.box) field
val hidden   : ?json:('cfg -> bool) -> name:string -> label:string -> ('cfg,View.Context.box) field

val select   :
     name:string
  -> label:string
  -> values:('cfg -> 'a list)
  -> renderer:(I18n.t -> 'a -> Json_type.t * View.Context.box View.t)
  -> ('cfg,View.Context.box) field

val radio    : name:string -> label:string -> values:('cfg -> 'a list) -> renderer:('a -> Json_type.t * I18n.text) ->
  ('cfg,View.Context.box) field

module Dyn : sig
  val text     : name:string -> label:('cfg -> I18n.text) -> ('cfg,View.Context.box) field
  val password : name:string -> label:('cfg -> I18n.text) -> ('cfg,View.Context.box) field
  val checkbox : name:string -> label:('cfg -> I18n.text) -> ('cfg,View.Context.box) field
  val textarea : name:string -> label:('cfg -> I18n.text) -> ('cfg,View.Context.box) field
end

val custom   : 
     ?json:bool
  ->  name:string 
  ->  label:string 
  ->  render:('cfg -> Id.t -> string -> View.Context.box View.t) 
  ->  ('cfg,View.Context.box) field

val add_js : (Id.t -> I18n.t -> JsCode.t) -> ('cfg,View.Context.box) field -> ('cfg,View.Context.box) field
val add_config_js : (Id.t -> I18n.t -> 'cfg -> JsCode.t)  -> ('cfg,View.Context.box) field -> ('cfg,View.Context.box) field

module type FORM = 
sig

  type config 
  val config  : config

  type t
  val of_json : Json_type.t -> t
  val to_json : t -> Json_type.t

  val fields  : t list
  val details : t -> (config,View.Context.box) field
    
  val hash    : (t -> Id.t) option

end

val prefixed_name_as_hash : string -> ('a -> ('cfg,'ctx) field) -> ('a -> Id.t) option

module Make :
  functor (Form:FORM) ->
sig

  type t

  val id : Form.t -> Id.t

  val empty : t
  val dynamic : Form.t list -> t 
  val initialize : ?dynamic:Form.t list -> (Form.t -> Json_type.t) -> t
  val parse : ?dynamic:Form.t list -> Json_type.t -> t

  val get_field_json : t -> Form.t -> Json_type.t option   

  val fold : (Form.t -> Json_type.t -> 'a -> 'a) -> t -> 'a -> 'a

  val readpost : ?dynamic:Form.t list -> (string -> string option) -> t 
  val response : t -> ( string * Json_type.t ) list

  val error : Form.t -> I18n.t * I18n.text -> ?cond:(Json_type.t -> bool) -> t -> t

  val mandatory : Form.t -> 'a Fmt.t -> 'a ref -> I18n.t * I18n.text -> t -> t
  val get : Form.t -> 'a Fmt.t -> ?error:(I18n.t * I18n.text) -> t -> 'a option * t
  val map : Form.t -> 'a Fmt.t -> ('a -> 'b option) -> ?error:(I18n.t * I18n.text) -> t -> 'b option * t
  val breathe : Form.t -> 'a Fmt.t -> ('a -> ('ctx,'b option) Run.t) -> ?error:(I18n.t * I18n.text) -> t -> ('ctx,'b option * t) Run.t
  val optional : Form.t -> 'a Fmt.t -> 'a option ref -> t -> t

  val is_valid : t -> bool
  val not_valid : t -> bool
    
  val to_mapping : 
        prefix:string 
    ->  url:('arg -> string)
    ->  init:('arg -> t)
    -> ?id:('arg -> Id.t)
    -> ?config:('arg -> Form.config)
    -> ?dynamic:('arg -> Form.t list)
    ->  (string * (('arg,View.Context.box) Template.t -> ('arg,View.Context.box) Template.t)) list 
    ->  (string * (('arg,View.Context.box) Template.t -> ('arg,View.Context.box) Template.t)) list

  class dyn : Form.config -> I18n.t -> object
    method input : Form.t -> View.Context.box View.t
    method label : Form.t -> View.Context.box View.t 
    method error : Form.t -> View.Context.box View.t      
  end

end
