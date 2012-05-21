(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives

type t = Json_type.t = 
  | Null
  | Array of t list
  | Object of (string * t) list
  | Float of float
  | Int of int
  | Bool of bool
  | String of string

exception Error = Json_type.Error

let of_json x = x
let to_json x = x

let unserialize string =
  let lexbuf = Lexing.from_string string in
    try Json_lex.value lexbuf
    with _ -> let s = String.sub
		lexbuf.Lexing.lex_buffer
		lexbuf.Lexing.lex_start_pos
		(String.length lexbuf.Lexing.lex_buffer - lexbuf.Lexing.lex_start_pos)
	      in
	      raise (Json_lex.unexpected s)

let rec debug = function
  | Null  -> "Null"
  | Int i -> "Int " ^ string_of_int i 
  | Float f -> "Float " ^ string_of_float f
  | String s -> "String " ^ Printf.sprintf "%S" s
  | Bool true -> "Bool true"
  | Bool false -> "Bool false"
  | Array l -> "Array [ " ^ String.concat " ; " (List.map debug l) ^ " ]"
  | Object o -> "Object [ " ^ String.concat " ; " 
    (List.map (fun (k,v) -> Printf.sprintf "%S, %s" k (debug v)) o) ^ " ]"

let serialize json =
  let buffer = Buffer.create 1024 in
  let rec value = function
    | Json_type.String s   -> string s
    | Json_type.Null       -> Buffer.add_string buffer "null"
    | Json_type.Bool true  -> Buffer.add_string buffer "true"
    | Json_type.Bool false -> Buffer.add_string buffer "false"
    | Json_type.Int i      -> Buffer.add_string buffer (string_of_int i)
    | Json_type.Float f    -> let f = string_of_float f in
			      if f.[String.length f - 1] = '.' then
				Buffer.add_substring buffer f 0 (String.length f - 1)
			      else
				Buffer.add_string buffer f
    | Json_type.Array []   -> Buffer.add_string buffer "[]"
    | Json_type.Object []  -> Buffer.add_string buffer "{}"

    | Json_type.Array (h :: t) ->
      Buffer.add_char buffer '[' ;
      value h ;
      List.iter (fun x -> Buffer.add_char buffer ',' ; value x) t ;
      Buffer.add_char buffer ']'
	
    | Json_type.Object (h :: t) ->
      Buffer.add_char buffer '{' ;
      pair h ;
      List.iter (fun x -> Buffer.add_char buffer ',' ; pair x) t ;
      Buffer.add_char buffer '}'
  and pair (k,v) =
    string k ;
    Buffer.add_char buffer ':' ;
    value v
  and string s =
    Buffer.add_char buffer '"' ;
    clean s 0 0 (String.length s);
    Buffer.add_char buffer '"'
  and clean s prev n m =
    if m = n then
      if prev < n then Buffer.add_substring buffer s prev (n-prev) else ()
    else
      match s.[n] with
	| '\"' ->
	  if prev < n then Buffer.add_substring buffer s prev (n-prev) ;
	  Buffer.add_string buffer "\\\"" ;
	  clean s (n+1) (n+1) m
	| '\n' ->
	  if prev < n then Buffer.add_substring buffer s prev (n-prev) ;
	  Buffer.add_string buffer "\\n" ;
	  clean s (n+1) (n+1) m
	| '\b' ->
	  if prev < n then Buffer.add_substring buffer s prev (n-prev) ;
	  Buffer.add_string buffer "\\b" ;
	  clean s (n+1) (n+1) m
	| '\t' ->
	  if prev < n then Buffer.add_substring buffer s prev (n-prev) ;
	  Buffer.add_string buffer "\\t" ;
	  clean s (n+1) (n+1) m
	| '\r' ->
	  if prev < n then Buffer.add_substring buffer s prev (n-prev) ;
	  Buffer.add_string buffer "\\r" ;
	  clean s (n+1) (n+1) m
	| '\\' ->
	  if prev < n then Buffer.add_substring buffer s prev (n-prev) ;
	  Buffer.add_string buffer "\\\\" ;
	  clean s (n+1) (n+1) m
	| _ -> clean s prev (n+1) m
  in
  
  value json ;
  Buffer.contents buffer
     
let of_assoc  list   = Object list
let of_int    int    = Int int
let of_float  float  = Float float
let of_string string = String string
let of_bool   bool   = Bool bool
let of_array  list   = Array list
let of_opt    f x    = BatOption.default Null (BatOption.map f x) 
let of_list   f list = Array (List.map f list) 

let parse_error what json = 
  let string = serialize json in 
  raise (Error (Printf.sprintf "Expected %s, found `%s`" what string))  

let to_object f = function
  | Object list -> let opt s = try Some (List.assoc s list) with Not_found -> None in
		   let req s = try List.assoc s list with Not_found -> 
		     parse_error ("object with key `" ^ s ^ "`") (Object list) in
		   f ~opt ~req
  | json -> parse_error "object" json

let to_list f = function 
  | Array list -> List.map f list
  | json       -> parse_error "array" json

let to_int = function 
  | Int i -> i 
  | json  -> parse_error "int" json 

let to_float = function
  | Float f -> f
  | Int   i -> float_of_int i
  | json    -> parse_error "float" json 

let to_string = function
  | String s -> s
  | json     -> parse_error "string" json 

let to_bool = function
  | Bool b -> b
  | json   -> parse_error "bool" json

let to_opt f = function 
  | Null -> None
  | json -> Some (f json) 
 
let parse f json = 
  try Ok (f json) with 
    | Error e -> Bad (Error (e ^ " in " ^ to_string json)) 
    | exn -> Bad exn 

let to_array = function
  | Array a -> a
  | json -> parse_error "array" json

let to_assoc = function
  | Object o -> o 
  | json -> parse_error "object" json
