(* Ohm is © 2011 Victor Nicollet *)

(** The type of a field identifier. These are provided to the deserialization functions 
    defined in the template, and can be used to add errors to those fields in the 
    form response.
*)
type field

(** A CSS selector used by the javascript code to select a piece of the generated HTML to
    bind behavior to it. Most of the rendering of Joy forms involves providing an HTML 
    renderer and a set of selectors to pick the relevant items from within it. Those 
    selectors are relative to the HTML's root, not the root of the document.
*)
type selector = string

(** A selector equal to [""], which the JS library interprets as "current context". *)
val here : selector

(** Describes the structure of a form. Forms can be initialized from an application-provided
    type called the seed (the first type parameter) and can be parsed into an 
    application-provided type called the result (the second type parameter).
*)
type ('seed,'result) template

(** Applies a map function to the seed part of the template *)
val seed_map : ('a -> 'b) -> ('b,'result) template -> ('a,'result) template

(** Applies a map function t the result part of the template *)
val result_map : ('a -> 'b) -> ('seed,'a) template -> ('seed,'b) template

(** A template that always returns the same result. *)
val constant : 'result -> ('seed,'result) template

(** Specify the HTML context for this object. *)
val wrap :
  selector ->
  (I18n.t -> View.Context.box View.t) ->
  ('seed,'result) template ->
  ('seed,'result) template

(** A single string field.
    
    Rendering involves several HTML contexts: the parent context is
    the HTML of the parent Joy template (or the Joy root, if none), the
    field context is the context of the input element's HTML (if missing, use the
    full context) and the label context is the context of the label element's HTML 
    (if missing, use the full context).
    
    Missing HTML fields have no effect. If the label parameter is not provided, the
    label HTML is not rendered.
    
    @param field Find the field element within the field context.
    @param label_html Find a location within the parent context and append the label HTML there.
    @param field_html Find a location within the parent context and append the field HTML there.
    @param error_html Find a location within the parent context and append the error HTML there.
    @param label Find the label element within the label context and render the text there.
    @param error Find the error label element within the error context.
*)
val string : 
  field:selector ->
 ?label_html:(selector * (I18n.t -> View.Context.box View.t)) ->
 ?field_html:(selector * (I18n.t -> View.Context.box View.t)) ->
 ?error_html:(selector * (I18n.t -> View.Context.box View.t)) ->
 ?label:(selector * I18n.text) ->
 ?error:selector ->
  (I18n.t -> 'seed -> string) ->
  (I18n.t -> field -> string -> ('result,field * I18n.text) BatStd.result) ->
  ('seed,'result) template

(** A JSON selector/autocomplete field.

    Rules for rendering are (almost) the same as for the normal string selector.
    
    Instead of reading and writing string, the selector relies on a JSON formatter to
    read and write values of an arbitrary type. 

    The list of available values is provided by one or two sources. A static source
    is a list of values with their labels and (optional) HTML. A dynamic source
    is an URL which, when provided with a prefix (sent under the GET parameter "term"),
    returns an appropriately formatted list of values-labels-(optional)-HTML.
    
    @param field Find the field element within the field context.
    @param label_html Find a location within the parent context and append the label HTML there.
    @param field_html Find a location within the parent context and append the field HTML there.
    @param error_html Find a location within the parent context and append the error HTML there.
    @param label Find the label element within the label context and render the text there.
    @param error Find the error label element within the error context.
    @param format The JSON formatter to be used.
    @param static The static source.
    @param dynamic The dynamic source.
*)
val select : 
  field:selector ->
 ?label_html:(selector * (I18n.t -> View.Context.box View.t)) ->
 ?field_html:(selector * (I18n.t -> View.Context.box View.t)) ->
 ?error_html:(selector * (I18n.t -> View.Context.box View.t)) ->
 ?label:(selector * I18n.text) ->
 ?error:selector ->
  format:'data Fmt.fmt ->
  source:[`Static of ('data * I18n.text * (I18n.t -> View.Context.text View.t) option) list 
	 |`Dynamic of string
	 |`Both of ('data * I18n.text * (I18n.t -> View.Context.text View.t) option) list * string] ->
  (I18n.t -> 'seed -> 'data option) ->
  (I18n.t -> field -> 'data option  -> ('result,field * I18n.text) BatStd.result) ->
  ('seed,'result) template


(** A JSON single/multiple-select field.

    Rules for rendering are (almost) the same as for the normal string selector.
    
    Instead of reading and writing string, the selector relies on a JSON formatter to
    read and write values of an arbitrary type. 

    The list of available values is provided by one or two sources. A static source
    is a list of values with their labels and (optional) HTML. A dynamic source
    is an URL which, when provided with a prefix (sent under the GET parameter "term"),
    returns an appropriately formatted list of values-labels-(optional)-HTML.
    
    @param field Find the field element within the field context.
    @param label_html Find a location within the parent context and append the label HTML there.
    @param field_html Find a location within the parent context and append the field HTML there.
    @param error_html Find a location within the parent context and append the error HTML there.
    @param label Find the label element within the label context and render the text there.
    @param error Find the error label element within the error context.
    @param format The JSON formatter to be used.
*)
val choice : 
  field:selector ->
 ?label_html:(selector * (I18n.t -> View.Context.box View.t)) ->
 ?field_html:(selector * (I18n.t -> View.Context.box View.t)) ->
 ?error_html:(selector * (I18n.t -> View.Context.box View.t)) ->
 ?label:(selector * I18n.text) ->
 ?error:selector ->
  format:'data Fmt.fmt ->
  source:('data * (I18n.t -> View.Context.text View.t)) list ->
  multiple:bool ->
  (I18n.t -> 'seed -> 'data list) ->
  (I18n.t -> field -> 'data list  -> ('result,field * I18n.text) BatStd.result) ->
  ('seed,'result) template


(** An array.

    Two HTML contexts are involved: the root context, which contains an ul-type element
    (ul, ol, table, div...) and an item context, which is usually the li-type element
    (li, tr, div...) .    

    @param list A selector for finding the ul-type element.
    @param add A selector for finding the "add" link/button.
    @param item Renders an item.
    @param remove A selector for finding the "remove" link/button.
*)

val array : 
  list:selector ->
  add :selector ->
  item:(I18n.t -> View.Context.box View.t) ->
  remove:selector ->
  ('seed,'result) template ->
  ('seed list,'result list) template 

(** An optional value. 

    Two HTML contexts are involved: the root context, which contains an ul-type element
    (ul, ol, table, div...) and an item context, which is usually the li-type element
    (li, tr, div...). The system works exactly like an array, but only one value may be
    added. 

    @param list A selector for finding the ul-type element.
    @param add A selector for finding the "add" link/button.
    @param item Renders an item.
    @param remove A selector for finding the "remove" link/button.
*)

val option : 
  list:selector ->
  add :selector ->
  item:(I18n.t -> View.Context.box View.t) ->
  remove:selector ->
  ('seed,'result) template ->
  ('seed option,'result option) template 
    
(** Extracts the search parameter for a JSON selector.

    This merely reads out the "term" GET parameter.
*)
val select_search_param : 
  'data Fmt.fmt ->
  < post : string -> string option ; .. > -> 
  [ `Complete of string | `Get of 'data ]

(** Return a formatted list of search results for a JSON selector. *)
val select_return_list : 
  'data Fmt.fmt -> I18n.t ->
  ('data * I18n.text * (I18n.t -> View.Context.text View.t) option) list ->
  (string * Json_type.t) list

(** A field concatenator.
    
    Appends field A to field B and constructs a new result from their combined results using
    a combinator function. May optionally define a brand new context, otherwise the parent
    rendering context is used by both fields.    
*)
val append : 
  ('a -> 'b -> 'c) ->
  ('seed,'b) template ->
  ('seed,'a) template ->
  ('seed,'c) template

(** Begin specifying an object.

    This template is provided as a helper to which one can append the fields of an object.
    It returns the result provided upon creation, which is usually a function with labels,
    so that every appended field can use a combinator like [fun f a -> f ~a], making it
    easier to combine all the fields into one object.

    As you might have guessed, this is an alias for [constant]
*)
val begin_object : 'result -> ('seed,'result) template

(** Finish specifying an object. 

    This optionally sets some HTML for an object to be rendered into.

    This is nearly an alias for [html], the difference being that the HTML is optional.
*)
val end_object :
  ?html:(selector * (I18n.t -> View.Context.box View.t)) ->
  ('seed,'result) template ->
  ('seed,'result) template

(** Provides a source of data for initializing a form during construction. *)
type 'seed source 

(** Initialize the form with all empty fields. *)
val empty : 'any source 

(** Initialize the form with all empty fields, but carrying a parametric payload that
    will be available on submission.
*)
val from_params : Json_type.t -> 'any source

(** Initialize the form using an application-provided type. This is usually performed when
    displaying a form for the first time, using database-provided data. 
*)
val from_seed : ?params:Json_type.t -> 'seed -> 'seed source

(** Initialize the form using a string sent by the client. This is usually performed when
    parsing the client-sent response. Note that the string contains JSON data serialized
    by the client.
*)
val from_post : string -> 'any source

(** Initialize the form using a JSON value. This function is called by {!from_response} 
    internally, but it may be used when the client response was sent as part of a
    larger JSON value and is already deserialized.
*)
val from_post_json : Json_type.t -> 'any source

(** A form instance, carries some data which may be extracted into the result type. *)
type 'result form

(** Creating a specific form instance. *)
val create : 
     template:('seed,'result) template
  -> i18n:I18n.t
  -> source:'seed source
  -> 'result form

(** Render the form HTML, outputs both HTML and JavaScript. The URL to which the form must post
    is provided as well.
*)
val render : 'result form -> string -> View.Context.box View.t

(** Compute the server's JSON response. Includes both the form data and any specified errors. *)
val response : 'result form -> (string * Json_type.t) list

(** Extract the parameters from a form. *)
val params : 'result form -> Json_type.t

(** Alter the form by adding errors. These are provided as an associative list. Only the last
    error for any field is kept. The new form is returned.
*)
val set_errors : (field * I18n.text) list -> 'result form -> 'result form

(** Determine whether a form contains any errors. *)
val has_errors : 'result form -> bool

(** Extract the form data as a result type. This uses the parsing functions provided in the 
    template. Usually, the result type is either a variant that can represent a correct value
    OR a list of errors, or a function that returns such a variant. Use of BatStd.result is
    suggested.
*)
val result : 'result form -> ('result, (field * I18n.text) list) BatStd.result

