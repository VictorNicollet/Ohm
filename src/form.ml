(* Ohm is © 2011 Victor Nicollet *)

open Util
open BatPervasives
open Universal

type ('cfg,'ctx) field = 
    {
      name   : string ; 
      render : Id.t -> I18n.t -> 'cfg -> 'ctx View.t ;
      label  : 'cfg -> I18n.text ;
      json   : 'cfg -> bool
    }

let field ?json ~name ~render ~label () = { 
  name   = name ;
  render = render ;
  label  = label ;
  json   = match json with Some f -> f | None -> (fun _ -> false)
}

module Dyn = struct

  let text ~name ~label = 
    let render id _ _ ctx = ctx
      |> View.str "<input type='text' name='"
      |> View.esc name
      |> View.str "' id='"
      |> View.esc (Id.str id)
      |> View.str "'/>"
    in
    field ~render ~name ~label () 

  let password ~name ~label = 
    let render id _ _ ctx = ctx
      |> View.str "<input type='password' name='"
      |> View.esc name
      |> View.str "' id='"
      |> View.esc (Id.str id)
      |> View.str "'/>"
    in
    field ~render ~name ~label ()
      
  let checkbox ~name ~label = 
    let render id _ _ ctx = ctx 
      |> View.str "<input type='checkbox' name='"
      |> View.esc name
      |> View.str "' id='"
      |> View.esc (Id.str id)
      |> View.str "'/>"
    in
    field ~render ~name ~label ()

  let textarea ~name ~label =
    let render id _ _ ctx = ctx
      |> View.str "<textarea name='"
      |> View.esc name
      |> View.str "' id='"
      |> View.esc (Id.str id)
      |> View.str "'></textarea>"
    in
    field ~render ~name ~label ()

end
  
let text     ~name ~label = Dyn.text     ~name ~label:(fun _ -> `label label)
let password ~name ~label = Dyn.password ~name ~label:(fun _ -> `label label)
let checkbox ~name ~label = Dyn.checkbox ~name ~label:(fun _ -> `label label)
let textarea ~name ~label = Dyn.textarea ~name ~label:(fun _ -> `label label)

let select ~name ~label ~values ~renderer = 
  let render id i18n config ctx = ctx 
    |> View.str ("<select name='"^name^"' id='"^(Id.str id)^"'>")
    |> View.foreach (fun v ctx -> 
      let value, label = renderer i18n v in
      ctx
      |> View.str "<option value='"
      |> View.esc (Json_io.string_of_json ~recursive:true ~compact:true value)
      |> View.str "'>"
      |> label
      |> View.str "</option>") (values config)
    |> View.str "</select>"
  in
  field ~json:(fun _ -> true) ~render ~name ~label:(fun _ -> `label label) ()

let radio ~name ~label ~values ~renderer = 
  let render id i18n config ctx = ctx 
    |> View.foreach (fun (n,v) ctx -> 
      let id = if n = 0 then Id.str id else Id.str id ^ "-" ^ string_of_int n in
      let value, label = renderer v in
      ctx
      |> View.str ("<div class='radio'><input type='radio' name='"^name^"' id='"^id^"' value='")
      |> View.esc (Json_io.string_of_json ~recursive:true ~compact:true value)
      |> View.str ("'/><label class='label' for='"^id^"'>") 
      |> I18n.get i18n label
      |> View.str "</label></div>") (Util.number (values config))
  in
  field ~json:(fun _ -> true) ~render ~name ~label:(fun _ -> `label label) ()

let hidden ?json ~name ~label = 
  let render id _ _ = View.str ("<input type='hidden' name='"^name^"' id='"^(Id.str id)^"'/>") in
  field ?json ~render ~name ~label:(fun _ -> `label label) ()

let custom ?json ~name ~label ~render = 
  let render id _ cfg ctx = render cfg id name ctx in
  let json = BatOption.map (fun x _ -> x) json in
  field ?json ~render ~name ~label:(fun _ -> `label label) ()

let add_js js field = 
  { field with render = fun id i18n cfg c -> field.render id i18n cfg c |> View.Context.add_js_code (js id i18n) }

let add_config_js js field = 
  { field with render = fun id i18n cfg c -> field.render id i18n cfg c |> View.Context.add_js_code (js id i18n cfg) }

module type FORM = 
sig

  type config
  val config  : config

  type t
  val of_json : Json_type.t -> t
  val to_json : t -> Json_type.t

  val fields  : t list
  val details : t -> (config,View.Context.box) field

  val hash : (t -> Id.t) option

end

let prefixed_name_as_hash name details = 
  Some (fun t -> Id.of_string (name ^ "--" ^ (details t).name))

module Make = 
  functor (Form:FORM) ->
struct

  let _hash = 
    match Form.hash with 
      | Some hash -> hash 
      | None -> begin
	let known = Hashtbl.create (List.length Form.fields) in
	function field -> 
	  try Hashtbl.find known field 
	  with Not_found ->
	    let id = Id.gen () in 
	    Hashtbl.add known field id ; 
	    id
      end

  let _json field config = 
    let details = Form.details field in 
    details.json config

  let id field = _hash field
    
  type t = {
    fields : Form.t list ;
    values : (Form.t, Json_type.t) BatMap.t ;
    errors : (Form.t, string) BatMap.t ;
  }

  let empty = {
    fields = Form.fields ;
    values = BatMap.empty ;
    errors = BatMap.empty ;
  }

  let dynamic d = { empty with fields = d @ Form.fields }

  let initialize ?(dynamic = []) f = 
    let fields = dynamic @ Form.fields in 
    { 
      fields = fields ;
      values = 
	List.fold_left 
	  (fun values field -> BatMap.add field (f field) values)
	  BatMap.empty fields ;
      errors = BatMap.empty
    }

  let _parse json = 
    List.fold_left begin fun values line ->
      try 
	match Json_type.Browse.array line with
	  | field :: value :: _ ->
	    BatMap.add (Form.of_json field) value values
	  | _ -> values
      with _ -> values
    end BatMap.empty (Json_type.Browse.array json) 

  let parse ?(dynamic=[]) json = try {
    fields = (dynamic @ Form.fields) ;
    values = _parse json ;
    errors = BatMap.empty
  } with _ -> empty

  let readpost ?dynamic request = 
    try 
      match request "data" with 
	| Some json -> parse ?dynamic (Json_io.json_of_string json)
	| None -> empty
    with  _ -> empty

  let get_field_json form field = 
    try Some (BatMap.find field form.values) with Not_found -> None

  let fold func form init = 
    BatMap.foldi func form.values init 

  let error field (i18n,key) ?(cond=(fun _ -> true)) form = 
    match get_field_json form field with 
      | Some value -> 
	if cond value then 
	  let text = View.write_to_string (I18n.get i18n key) in
	  { form with errors = BatMap.add field text form.errors }
	else
	  form
      | None -> form

  let mandatory field fmt into (i18n,key) form = 
    let missing = 
      match get_field_json form field with 
	| Some (Json_type.Object []) 
	| Some (Json_type.Array []) 
	| Some (Json_type.String "")
	| Some (Json_type.Null)
	| None -> true
	| Some value -> begin
	  match fmt.Fmt.of_json value with
	    | Some result -> into := result ; false
	    | None -> true
	end
    in  
    if missing then 
      let text = View.write_to_string (I18n.get i18n key) in 
      { form with errors = BatMap.add field text form.errors }
    else form

  let optional field fmt into form =
    let () =       
      match get_field_json form field with 
	| Some (Json_type.Object []) 
	| Some (Json_type.Array []) 
	| Some (Json_type.String "")
	| Some (Json_type.Null)
	| None -> () 
	| Some value -> begin
	  match fmt.Fmt.of_json value with	     
	    | Some result -> into := Some result
	    | None -> ()
	end
    in form

  let get field fmt ?error form = 
    let value = 
      match get_field_json form field with 
	| Some (Json_type.Object []) 
	| Some (Json_type.Array []) 
	| Some (Json_type.String "")
	| Some (Json_type.Null)
	| None -> None
	| Some value -> fmt.Fmt.of_json value 
    in
    if value = None then 
      match error with 
	| None -> None, form 
	| Some (i18n, key) -> 
	  None, { form with errors = BatMap.add field (I18n.translate i18n key) form.errors }
    else
      value, form

  let map field fmt map ?error form = 
    let value, form = get field fmt ?error form in
    match value with None -> None, form | Some value ->
      match error with None -> map value, form | Some (i18n, key) ->
	match map value with Some result -> Some result, form | None ->
	  None, { form with errors = BatMap.add field (I18n.translate i18n key) form.errors }

  let breathe field fmt map ?error form = 
    let value, form = get field fmt ?error form in 
    match value with None -> Run.return (None, form) | Some value ->
      Run.map begin function
	| None -> None,
	  ( match error with 
	    | None -> form
	    | Some (i18n,key) -> { form with errors = BatMap.add field (I18n.translate i18n key) form.errors } )
	| some -> some, form
      end  (map value)

  let is_valid form = 
    BatMap.is_empty form.errors

  let not_valid form = 
    not (BatMap.is_empty form.errors)

  let _response t = 
    let values = 
      BatMap.foldi begin fun field value list ->
	(Id.str (_hash field) , value) :: list
      end t.values []	
    and errors = 
      BatMap.foldi begin fun field error list ->
	(Id.str (_hash field) , Json_type.Build.string error) :: list
      end t.errors []
    in
    Json_type.Build.objekt [
      "values", Json_type.Build.objekt values ;
      "errors", Json_type.Build.objekt errors
    ]  

  let response t = [ "form" , _response t ]

  let to_mapping ~prefix ~url ~init ?id ?config ?dynamic mapping = 
    let get_data = init in 
    let dynamic  = match dynamic with None -> (fun _ -> []) | Some f -> f in
    let get_id = match id with Some id -> id | None -> let id = Id.gen () in fun _ -> id in
    let prefix = if prefix = "" then "form." else prefix ^ "." in
    let mapping = 
      let js arg =
	let field_json = 
	  let cfg = match config with Some cfg -> cfg arg | None -> Form.config in
	  List.map begin fun field ->
	    Id.str (_hash field),
	    Json_type.Build.objekt (
	      ("n",Form.to_json field) :: (if _json field cfg then ["json",Json_type.Build.bool true] else [])
	    )
	  end ((dynamic arg) @ Form.fields)
	  |> Json_type.Build.objekt
	in 
	JsBase.form (get_id arg) field_json (_response (get_data arg))
      in

      ( prefix ^ "begin", (fun t -> t
	|> Template.Mk.put "<form id='"
	|> Template.Mk.esc (fun d -> Id.str (get_id d))
	|> Template.Mk.put "' action='"
	|> Template.Mk.esc url 
	|> Template.Mk.put "'>"))

      :: (prefix ^ "end", (fun t -> t 
	|> Template.Mk.put  "</form>"
	|> Template.Mk.code js))

      :: mapping
    in
    List.fold_left begin fun list field ->
      let detail = Form.details field in 
      let name   = prefix ^ detail.name 
      and label  = detail.label 
      and render = detail.render 
      and id     = _hash field in

      (name ^ ".label", (fun t -> t
	|> Template.Mk.put ("<label class='label' for='"^Id.str id^"'>" )
	|> Template.Mk.trad (fun d -> label (match config with Some cfg -> cfg d | None -> Form.config))
	|> Template.Mk.put "</label>" ))

      :: (name ^ ".input", Template.Mk.ihtml begin fun d i18n ctx -> 
	render id i18n (match config with Some cfg -> cfg d | None -> Form.config) ctx
      end)

      :: (name ^ ".error", Template.Mk.put
	("<label for='" ^ (Id.str id) ^ "' style='display:none' class='field-error'></label>"))

      :: list
    end mapping 
      (* Only static fields included in mapping, of course : templates ARE static. *)
      Form.fields 

  class dyn config i18n = object (self)

    val config = config
    val i18n   = i18n 

    method label field (ctx : View.Context.box) = 
      let detail = Form.details field in 
      let label  = detail.label config in
      let id     = _hash field in 
      ctx 
      |> View.str ("<label class='label' for='" ^ Id.str id ^ "'>") 
      |> I18n.get i18n label
      |> View.str "</label>"

    method input field ctx = 
      let detail = Form.details field in 
      let render = detail.render in
      let id     = _hash field in 
      render id i18n config ctx

    method error field (ctx : View.Context.box) = 
      let id     = _hash field in 
      View.str ("<label for='" ^ Id.str id ^ "' style='display:none' class='field-error'></label>") ctx
      
  end

end
