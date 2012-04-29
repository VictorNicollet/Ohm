(* Ohm is © 2011 Victor Nicollet *)

open Util
open BatPervasives
open Universal

module Dictionary = Fmt.Make(struct
  type json t = < 
    volatile : bool ; 
    parent   : Id.t option ; 
    data     : (string, string) Hashtbl.t 
  > 
end)

module Source = JoyA.Make(struct
  let edit = JoyA.dict (JoyA.string ())
  type json t = (string * string) assoc
end)

module Text = Fmt.Make(struct
  type json t = [ `text "t" of string | `label "l" of string ]
end)

type text = Text.t

type elem = 
  | String of string
  | Param  of int

type language = [ `Fr ]

let languages = [ `Fr ]

type t = 
    {
      parent : t option ;
      static : (string,string) Hashtbl.t ;
      param  : (string,elem list) Hashtbl.t ;
      lang   : language ;
    }

type html = t -> View.html

let empty lang = 
  { parent = None ; static = Hashtbl.create 1 ; param = Hashtbl.create 1 ; lang = lang }

let elem_source elems = 
  String.concat "" (List.map (function
    | String s -> s
    | Param p -> "#" ^ string_of_int (p+1) ^ "#") elems)

let source t = 
  let static = Hashtbl.fold (fun k v l -> (k,v) :: l) t.static [] in
  let all = Hashtbl.fold (fun k v l -> (k, elem_source v) :: l) t.param static in 
  List.sort compare all

let _parse ?parent hash lang = 
  let out = { parent = parent ; static = Hashtbl.create 100 ; param = Hashtbl.create 100 ; lang = lang } in
  Hashtbl.iter begin fun k v ->
    match Str.full_split (Str.regexp "#[1-9][0-9]*#") v with
      | [Str.Text x] -> Hashtbl.add out.static k v
      | list -> List.map begin function 
	  | Str.Delim s -> (try Param (int_of_string (String.sub s 1 (String.length s - 2)) - 1) with _ -> String s)
	  | Str.Text  s -> String s
      end list 
        |> List.filter (fun s -> s <> String "")      
	|> Hashtbl.add out.param k
  end hash ; out

let translate i18n key = 
  match key with `text t -> t | `label key -> 
    if key = "" then "" else
      let rec aux i18n = 
	try Some (Hashtbl.find i18n.static key) 
	with Not_found -> BatOption.bind aux i18n.parent
      in match aux i18n with 
	| Some str -> str
	| None     -> "{!"^key^"!}"

let get i18n key ctx = 
  View.esc (translate i18n key) ctx

let label key i18n ctx = get i18n (`label key) ctx

let get_param i18n key params ctx = 
  let rec aux i18n = 
    try Some (Hashtbl.find i18n.param key) 
    with Not_found -> BatOption.bind aux i18n.parent
  in match aux i18n with 
    | None -> 
      ctx 
      |> View.str ("{!"^key^" ")
      |> View.implode (View.str " ") (fun s -> s) params
      |> View.str "!}"
    | Some list -> 
      List.fold_left begin fun ctx l -> match l with 
	| String str -> View.str str ctx
	| Param i -> 
	  try BatList.at params i ctx 
	  with _ -> View.str ("#"^string_of_int (i+1)^"#") ctx
      end ctx list
	
let language i18n = i18n.lang 

module Loader = 
  functor (DB:CouchDB.DATABASE) -> 
struct    

  module MyTable = CouchDB.Table(DB)(Id)(Dictionary) 

  let _cache = (Hashtbl.create 10 : (Id.t,t) Hashtbl.t)
    
  let _is_volatile id = 
    try let _ = Hashtbl.find _cache id in false with Not_found -> true
	    
  let rec _load id lang =
    try 
      (* Found in cache : return without accessing the database *)
      Hashtbl.find _cache id |> Run.return
    with Not_found -> 

      (* Assuming we retrieved a dictionary from the database, this is how we extract the full
	 dictionary chain (may involve loading other data from the database too, so this is
	 a monad.
      *)
      let get_chain =
	let process_parent data pid = 

	  let parse_parent (parent:t) = 
	    let _volatile = 		
	      if data # volatile then true 
	      else if _is_volatile pid then begin
		log "I18n.Loader.load : %s is non-volatile but has volatile parent %s" 
		  (Id.str id) (Id.str pid) ;
		true
	      end else false 		
	    and _chain = 
	      _parse ~parent (data # data) lang		
	    in	    
	    _volatile, _chain
	  in	  
	  _load pid lang |> Run.map parse_parent			    
	in

	let process data = 
	  match data # parent with Some pid -> process_parent data pid | None ->
	    Run.return (data # volatile, _parse (data # data) lang)
	in
	
	function Some data -> process data | None ->
	  log "I18n.Loader.load: could not find %s" (Id.str id) ;
	  Run.return (true, empty lang)
      in

      let cache (volatile,found) = 
	if not volatile then Hashtbl.add _cache id found ;
	found
      in

      MyTable.get id |> Run.bind get_chain |> Run.map cache      

  let load id lang = 
    Run.eval (new CouchDB.init_ctx) (_load id lang)

  let save id ?(volatile=false) ?parent source = 
    
    let data = Hashtbl.create 100 in

    List.iter (fun (key,value) -> Hashtbl.add data key value) source ;

    let obj = object
      method volatile = volatile
      method parent   = parent
      method data     = data
    end in

    MyTable.transaction id (MyTable.insert obj) |> Run.map ignore
    
      
  let copy ~src ~dest = 
    
    MyTable.get src |> Run.bind begin function
      | None     -> Run.return () 
      | Some src ->

	let data = src # data in 
	
	let update dest = object
	  method volatile = dest # volatile
	  method parent   = dest # parent
	  method data     = data
	end in
	
	MyTable.transaction dest (MyTable.update update) |> Run.map ignore
    end

end
