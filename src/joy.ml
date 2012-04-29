(* Ohm is © 2011 Victor Nicollet *)

open BatPervasives

type field = int list

type selector = string
let here = ""

type ('seed,'result) template = {
  template : I18n.t -> Json_type.t ;
  init : I18n.t -> 'seed -> Json_type.t ;
  parse : I18n.t -> Json_type.t -> field -> ('result,(field * I18n.text) list) BatStd.result ;  
}

let render_to_json view = 
  let html, js = View.extract view in 
  Json_type.Array [ Json_type.String html ;
		    JsBase.to_json (JsCode.seq [JsBase.staticInit ; js])]  

let seed_map f t =
  {
    template = t.template ;
    parse    = t.parse ;
    init     = (fun i18n x -> t.init i18n (f x)) ;
  }

let result_map f t = 
  {
    template = t.template ;
    init     = t.init ;
    parse    = (fun i18n json field -> 
      match t.parse i18n json field with Ok ok -> Ok (f ok) | Bad bad -> Bad bad
    )
  }

let constant value = 
  {
    template = (fun _ -> Json_type.Null) ;
    init     = (fun _ _ -> Json_type.Null) ;
    parse    = (fun _ _ _ -> Ok value) ;
  }

let wrap selector renderer t = 
  {
    t with 
      template = (fun i18n -> Json_type.Object [
	"t", Json_type.String "html" ;
	"s", Json_type.String selector ;
	"i", t.template i18n ;
	"h", render_to_json (renderer i18n) ;
      ])
  }
  
let string ~field ?label_html ?field_html ?error_html ?label ?error seed result = 
  {
    template = (fun i18n -> Json_type.Object (List.concat [
      
      [ "t", Json_type.String "string" ;
	"s", Json_type.String field ] ;
      
      ( match label_html with None -> [] | Some (sel,html) -> 
	[ "lh", render_to_json (html i18n) ;
	  "lhs", Json_type.String sel ] ) ;
      
      ( match field_html with None -> [] | Some (sel,html) ->
	[ "fh", render_to_json (html i18n) ;
	  "fhs", Json_type.String sel ] );
      
      ( match error_html with None -> [] | Some (sel,html) ->
	[ "eh", render_to_json (html i18n) ;
	  "ehs", Json_type.String sel ] );
      
      ( match label with None -> [] | Some (sel,text) ->
	[ "ls", Json_type.String sel ;
	  "lt", Json_type.String (I18n.translate i18n text) ] ) ;
      
      ( match error with None -> [] | Some sel ->
	[ "es", Json_type.String sel ] ) ;
      
    ])) ;

    init = (fun i18n source -> seed i18n source |> Json_type.Build.string ) ;
    
    parse = (fun i18n json field -> 
      let string = match json with Json_type.String s -> s | _ -> "" in
      match result i18n (List.rev field) string with Ok ok -> Ok ok | Bad b -> Bad [b]
    )
  }

let json_of_select_list fmt i18n list = 
  Json_type.Build.list (fun (value, label, html_opt) ->
    Json_type.Build.objekt ([
      "internal", fmt.Fmt.to_json value ;
      "value", Json_type.String (I18n.translate i18n label) ;
      "html", (match html_opt with
	| None -> Json_type.Null
	| Some html -> Json_type.String (View.write_to_string (html i18n))
      )
    ])
  ) list

let select_return_list fmt i18n list = 
  [ "list", json_of_select_list fmt i18n list ]

let select_search_param fmt request = 
  match request # post "complete" with 
    | Some term -> `Complete term
    | None -> 
      let get = 
	try 
	  match request # post "get" with 
	    | Some str -> let json = Json_io.json_of_string ~recursive:true str in
			  fmt.Fmt.of_json json
	    | None -> None
	with _ -> None
      in
      match get with 
	| Some value -> `Get value
	| None -> `Complete ""
			

let select
    ~field ?label_html ?field_html ?error_html ?label ?error
    ~format ~source
    seed result = 
  {
    template = (fun i18n -> Json_type.Object (List.concat [
      
      [ "t", Json_type.String "select" ;
	"s", Json_type.String field ] ;

      ( match source with 
	| `Static list -> [ "ss", json_of_select_list format i18n list ] ;
	| `Dynamic url -> [ "ds", Json_type.String url ] 
	| `Both (list,url) ->
	  [ "ss", json_of_select_list format i18n list ;
	    "ds", Json_type.String url ] ) ;
      
      ( match label_html with None -> [] | Some (sel,html) -> 
	[ "lh", render_to_json (html i18n) ;
	  "lhs", Json_type.String sel ] ) ;
      
      ( match field_html with None -> [] | Some (sel,html) ->
	[ "fh", render_to_json (html i18n) ;
	  "fhs", Json_type.String sel ] );
      
      ( match error_html with None -> [] | Some (sel,html) ->
	[ "eh", render_to_json (html i18n) ;
	  "ehs", Json_type.String sel ] );
      
      ( match label with None -> [] | Some (sel,text) ->
	[ "ls", Json_type.String sel ;
	  "lt", Json_type.String (I18n.translate i18n text) ] ) ;
      
      ( match error with None -> [] | Some sel ->
	[ "es", Json_type.String sel ] ) ;
      
    ])) ;

    init = (fun i18n source -> 
      match seed i18n source with 
	| None -> Json_type.Null
	| Some data -> format.Fmt.to_json data
    ) ;
    
    parse = (fun i18n json field -> 
      let data = format.Fmt.of_json json in 
      match result i18n (List.rev field) data with 
	| Ok ok -> Ok ok 
	| Bad b -> Bad [b]
    )
  }

let json_of_choice_list fmt i18n list = 
  Json_type.Build.list (fun (value, html) ->
    Json_type.Build.objekt ([
      "internal", fmt.Fmt.to_json value ;
      "html",     Json_type.String (View.write_to_string (html i18n))
    ])
  ) list

let choice
    ~field ?label_html ?field_html ?error_html ?label ?error
    ~format ~source ~multiple
    seed result = 
  {
    template = (fun i18n -> Json_type.Object (List.concat [
      
      [ "t",   Json_type.String "choice" ;
	"s",   Json_type.String field ;
	"src", json_of_choice_list format i18n source ;
        "m",   Json_type.Bool multiple ] ;

      ( match label_html with None -> [] | Some (sel,html) -> 
	[ "lh", render_to_json (html i18n) ;
	  "lhs", Json_type.String sel ] ) ;
      
      ( match field_html with None -> [] | Some (sel,html) ->
	[ "fh", render_to_json (html i18n) ;
	  "fhs", Json_type.String sel ] );
      
      ( match error_html with None -> [] | Some (sel,html) ->
	[ "eh", render_to_json (html i18n) ;
	  "ehs", Json_type.String sel ] );
      
      ( match label with None -> [] | Some (sel,text) ->
	[ "ls", Json_type.String sel ;
	  "lt", Json_type.String (I18n.translate i18n text) ] ) ;
      
      ( match error with None -> [] | Some sel ->
	[ "es", Json_type.String sel ] ) ;
      
    ])) ;

    init = (fun i18n source -> 
      Json_type.Build.list format.Fmt.to_json (seed i18n source)
    ) ;
    
    parse = (fun i18n json field -> 
      let data = 
	try let list = Json_type.Browse.array json in
	    BatList.filter_map format.Fmt.of_json list
	with _ -> []
      in 
      match result i18n (List.rev field) data with 
	| Ok ok -> Ok ok 
	| Bad b -> Bad [b] 
    )
  }

let array ~list ~add ~item ~remove inner = 
  {
    template = (fun i18n -> Json_type.Object 
      [ "t", Json_type.String "array" ;
	"ls", Json_type.String list ;
	"rs", Json_type.String remove ;
	"as", Json_type.String add ;
	"ih", render_to_json (item i18n) ;
	"i",  inner.template i18n 
      ]
    ) ;

    init = (fun i18n seed -> 
      Json_type.Array (List.map (inner.init i18n) seed)
    ) ;

    parse = (fun i18n json field ->
      let input = match json with Json_type.Array list -> list | _ -> [] in
      let results = 
	BatList.mapi (fun i json -> inner.parse i18n json (i :: field)) input 
      in
      if List.exists (function Bad _ -> true | Ok _ -> false) results then
	Bad (List.concat (BatList.filter_map (function
	  | Bad x -> Some x
	  | Ok  _ -> None) results
	))
      else
	Ok (BatList.filter_map (function
	  | Ok  x -> Some x
	  | Bad _ -> None) results
	)
    ) ;
  }     

let option ~list ~add ~item ~remove inner = 
  {
    template = (fun i18n -> Json_type.Object 
      [ "t",   Json_type.String "array" ;
	"ls",  Json_type.String list ;
	"rs",  Json_type.String remove ;
	"as",  Json_type.String add ;
	"ih",  render_to_json (item i18n) ;
	"max", Json_type.Int 1 ;
	"i",   inner.template i18n 
      ]
    ) ;

    init = (fun i18n seed -> 
      Json_type.Array (match seed with 
	| None       -> []
	| Some value -> [inner.init i18n value]
    )) ;

    parse = (fun i18n json field ->
      let input = match json with Json_type.Array (h :: _) -> Some h | _ -> None in
      let result = 
	BatOption.map (fun json -> inner.parse i18n json (0 :: field)) input 
      in
      match result with 
	| None         -> Ok None
	| Some (Ok  x) -> Ok (Some x)
	| Some (Bad x) -> Bad x
    ) ;
  }     
    
let append combine b a = 
  {
    template = (fun i18n -> Json_type.Array [
      a.template i18n ; b.template i18n
    ]) ;
    
    init = (fun i18n seed -> Json_type.Array [
      a.init i18n seed ; b.init i18n seed
    ]) ;
    
    parse = (fun i18n json field ->
      let a_json, b_json = match json with 
	| Json_type.Array (a :: b :: _) -> a, b 
	| Json_type.Array [a] -> a, Json_type.Null
	| _ -> Json_type.Null, Json_type.Null
      in
      let a_result = a.parse i18n a_json (0 :: field) in
      let b_result = b.parse i18n b_json (1 :: field) in
      match a_result, b_result with 
	| Ok a, Ok b -> Ok (combine a b)
	| Ok _, Bad l 
	| Bad l, Ok _ -> Bad l
	| Bad a, Bad b -> Bad (a @ b)
    ) ;
  }

let begin_object = constant

let end_object ?html template = 
  match html with None -> template | Some (sel,html) -> wrap sel html template

type 'seed source = 
  [ `Json of Json_type.t * Json_type.t
  | `Seed of 'seed * Json_type.t
  ]

let empty = `Json (Json_type.Null, Json_type.Null)

let from_params params = `Json (Json_type.Null, params)

let from_seed ?(params=Json_type.Null) seed = `Seed (seed,params) 

let from_post_json json = 
  try let assoc  = Json_type.Browse.objekt json in 
      let data   = List.assoc "data"   assoc in
      let params = List.assoc "params" assoc in 
      `Json (data,params)
  with _ -> empty

let from_post post = 
  try let json = Json_io.json_of_string ~recursive:true post in
      from_post_json json
  with _ -> empty

type 'result form = {
  result : ('result, (field * I18n.text) list) BatStd.result Lazy.t ;
  errors : (field * I18n.text) list ;
  i18n   : I18n.t ;
  config : Json_type.t ;
  data   : Json_type.t ;
  params : Json_type.t
}
    
let create ~template ~i18n ~source = 
  
  let data, params = match source with 
    | `Seed (seed,params) -> template.init i18n seed, params
    | `Json (json,params) -> json, params
  in
  
  {
    result = lazy (template.parse i18n data []) ;
    errors = [] ;
    i18n   = i18n ;
    config = template.template i18n ;
    data   ;
    params ;
  }

let params form = form.params

let render form url ctx =
  let id = Id.gen () in 
  ctx
  |> View.str "<form action=\""
  |> View.esc url 
  |> View.str "\" method=\"POST\"><input type=\"hidden\" id=\""
  |> View.esc (Id.str id) 
  |> View.str "\" value=\""
  |> View.esc (Json_io.string_of_json ~recursive:true ~compact:true form.data)
  |> View.str "\"/></form>"
  |> View.Context.add_js_code (JsCode.make ~name:"joy" ~args:[ Id.to_json id ; 
							       form.config ; 
							       form.params ])

let response form = 
  [
    "data", form.data ;
    "errors", Json_type.Build.list (fun (field,text) -> Json_type.Array [
      Json_type.Build.list Json_type.Build.int field ;
      Json_type.String (I18n.translate form.i18n text)
    ]) form.errors
  ] 

let set_errors errors form = 
  { form with errors = errors }

let has_errors form = form.errors <> []

let result form = Lazy.force form.result
