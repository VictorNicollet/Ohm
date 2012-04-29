(* Ohm is © 2011 Victor Nicollet *)

open Util

module Node = Fmt.Make(struct

  type json t = 
    [ `String  "string"  of string_node
    | `Bool    "bool"    of bool_node
    | `Dict    "dict"    of dict_node
    | `Option  "option"  of option_node
    | `Array   "array"   of array_node 
    | `Object  "object"  of object_node 
    | `Variant "variant" of variant_node 
    | `Label   "label"   of label_node
    | `Tuple   "tuple"   of tuple_node ]


  and dict_node = <
    content : t
  >

  and label_node = <
    label : string ;
    content : t
  >

  and bool_node = <
    ignored : int
  > 

  and option_node = <
    content : t
  >

  and tuple_node = <      
    fields : (string * t) list
  >

  and variant_node = <
    variants: (string * variant) assoc
  >

  and variant = <
    label : string ;
    content : t option 
  >

  and object_node = <
    fields : (string * field) assoc
  >

  and field = <
    label : string ;
    content : t 
  >

  and array_node = <
    sortable : bool ;
    validators : [ `min of int
 		 | `max of int
 		 ] list ;
    content : t 
  >

  and string_node = <
    autocomplete: string option ;
    editor: [ `line
	    | `area 
	    ] ;
    validators: [ `min of int 
		| `max of int
		] list 
  >
end)

type t = Node.t

type alternative = string * <
  label : string ;
  content : t option 
>

type field = string * <
  label : string ; 
  content : t 
>

let string ?(editor=`line) ?(validators=[]) ?autocomplete () = 

  let obj = object
    method autocomplete = autocomplete
    method editor      = editor
    method validators  = validators
  end in

  `String obj

let array ?(sortable=true) ?(validators=[]) content = 

  let obj = object
    method sortable   = sortable
    method validators = validators
    method content    = content 
  end in 

  `Array obj

let dict content = 

  let obj = object
    method content = content
  end in 

  `Dict obj

let label label content = 

  let obj = object
    method label = label
    method content = content
  end in 

  `Label obj

let optional content = 
  
  let obj = object
    method content = content
  end in

  `Option obj

let bool = 
  
  let obj = object
    method ignored = 0
  end in 

  `Bool obj

let field name ?label content = 

  let obj = object
    method label   = BatOption.default name label
    method content = content
  end in 

  (name, obj)

let obj fields = 

  let obj = object
    method fields = fields
  end in 

  `Object obj

let alternative ?content ?label name = 
  
  let obj = object
    method label   = BatOption.default name label
    method content = content
  end in 
  
  (name, obj)

let variant alternatives = 
  
  let obj = object
    method variants = alternatives
  end in 

  `Variant obj

let tuple fields = 

  let obj = object
    method fields = fields
  end in

  `Tuple obj

module type FMT = sig
  val edit : t
  include Fmt.FMT
end

module Make = functor(Type : sig
  val edit : t
  type t
  val t_of_json : Json_type.t -> t
  val json_of_t : t -> Json_type.t
end) -> struct

  let edit = Type.edit
  include Fmt.Make(Type)

end
