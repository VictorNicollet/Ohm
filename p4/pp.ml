open Camlp4.PreCast
open Syntax

(* [[    let! x = a in b    ]] *)

EXTEND Gram
    GLOBAL: expr;

    expr: LEVEL "top"
    [
      [ "let"; "!"; p = patt ; "=" ; e = expr ; "in" ; e' = expr ->
        <:expr< (bind ($e$) (fun $p$ -> $e'$)) >> ] 
    ] ;

END;; 


(* [[    List.map (#name) people    ]] *)

EXTEND Gram 
    GLOBAL: expr;

    expr: LEVEL "simple" 
    [
      [ "#" ; i = LIDENT ->
        <:expr< (fun __obj -> __obj # $i$) >> ]  
    ] ;

END;;

(* [[    type json person = < name : string >    ]] *)

type id = Loc.t * string

type typemod = Loc.t * id list 
type strtype = Loc.t * strtypedef
and strtypedef =
  [ `string
  | `m of typemod
  | `poly of < label : id ; name : id > list
  ] 

type typexpr = Loc.t * typedef
and typedef =
  [ `variant of < 
      label : id ;
      name  : id ;
      typ   : typexpr list 
    > list
  | `record  of < 
      label   : id ;
      name    : id ;
      typ     : typexpr ;      
      default : Ast.expr option option ;
      mut     : bool 
    > list
  | `tuple of typexpr list
  | `string
  | `bool
  | `float
  | `int 
  | `option of typexpr
  | `list   of typexpr
  | `array  of typexpr
  | `param  of typemod * typexpr
  | `param2 of typemod * strtype * typexpr
  | `obj    of <
      label   : id ;
      name    : id ;
      typ     : typexpr ; 
      default : Ast.expr option option ;
      mut     : bool 
    > list
  | `poly of <
      label : id ; 
      name  : id ;
      typ   : typexpr list
    > list 
  | `m of typemod
  ]

let def v = function 
  | None   -> v
  | Some v -> v

let loc   = fst
let ident = snd

let unique list = 
  let list = List.map (fun a -> a # label) list in
  let list = List.sort (fun a b -> compare (ident a) (ident b)) list in
  let rec test = function
    | [] | [_] -> () 
    | a :: (b :: _ as t) -> 
      if ident a = ident b then 
	Loc.raise (loc a) (Failure "This JSON label should be unique") ;
      test t
  in test list
	
let make_variant_bind ~name ?label ?(typ=[]) () = object

  val label = def name label 
  method label = label 

  val name = name
  method name = name

  val typ = typ
  method typ = typ

end

let make_poly_bind ~name ?label ?(typ=[]) () = object

  val label = def name label 
  method label = label 

  val name = name
  method name = name

  val typ = typ
  method typ = typ

end

let make_strpoly_bind ~name ?label () = object

  val label = def name label 
  method label = label 

  val name = name
  method name = name

end

let make_member_bind ~mut ~name ?label ?default typ = object 

  val label = def name label 
  method label = label 
    
  val mut = mut
  method mut = mut

  val name = name
  method name = name

  val default = default
  method default  = default

  val typ = typ
  method typ = typ

end

let generate_type _loc (def:typexpr) = 

  let in_module ((_loc,def):typemod) =
    let rec aux = function 
      | []     -> <:ident< t >>
      | h :: t -> let _loc = fst h in
		  <:ident< $uid:ident h$ . $aux t$ >> 
    in aux def
  in

  let strtype ((_loc,def):strtype) = match def with 
    | `string -> <:ctyp< string >>
    | `m m -> <:ctyp< $id:in_module m$ >> 
    | `poly p -> let l = List.fold_right begin fun ctor acc -> 
      
                   let _loc = loc (ctor # name) in
		   let name = ident (ctor # name) in
		   let ctor = <:ctyp< `$name$ >> in
		   <:ctyp< $ctor$ | $acc$ >>

                 end p <:ctyp< >> in 
		 <:ctyp< [ = $l$ ] >>
  in

  let rec recurse (_loc,def) = match def with 

    | `bool   -> <:ctyp< bool >>
    | `string -> <:ctyp< string >>
    | `int    -> <:ctyp< int >>
    | `float  -> <:ctyp< float >>

    | `list   t -> <:ctyp< list $recurse t$ >>
    | `array  t -> <:ctyp< array $recurse t$ >>
    | `option t -> <:ctyp< option $recurse t$ >>

    | `m m -> <:ctyp< $id:in_module m$ >> 

    | `record r -> let l = List.fold_right begin fun field acc -> 

                     let _loc = loc (field # name) in
		     let name = ident (field # name) in

                     let t = recurse (field # typ) in
		     let t = if field # mut then <:ctyp< mutable $t$ >> else t in
		     let field = <:ctyp< $lid:name$ : $t$ >> in

		     <:ctyp< $field$; $acc$ >>

                   end r <:ctyp< >> in 
		   <:ctyp< { $l$ } >>
		     
    | `obj o -> let l = List.fold_right begin fun field acc -> 

                  let _loc = loc (field # name) in
		  let name = ident (field # name) in
		  
                  let t = recurse (field # typ) in
		  let field = <:ctyp< $lid:name$ : $t$ >> in
		  
		  <:ctyp< $field$; $acc$ >>
		    
                end o <:ctyp< >> in 
		<:ctyp< < $l$ > >>
		  
    | `tuple t -> let t = List.fold_right begin fun t acc -> 
                    <:ctyp< $recurse t$ * $acc$ >>
                  end t <:ctyp< >> in
		  <:ctyp< ( $t$ ) >> 
		  
    | `variant v -> let l = List.fold_right begin fun ctor acc -> 
      
                      let _loc = loc (ctor # name) in
		      let name = ident (ctor # name) in
		      let ctor = match ctor # typ with 
			| [] -> <:ctyp< $uid:name$ >> 
			| l -> let l = List.map recurse l in
			       <:ctyp< $uid:name$ of $list:l$ >>
		      in 
		      <:ctyp< $ctor$ | $acc$ >>

                    end v <:ctyp< >> in 
		    <:ctyp< [ $l$ ] >>

    | `poly p -> let l = List.fold_right begin fun ctor acc -> 
      
                   let _loc = loc (ctor # name) in
		   let name = ident (ctor # name) in
		   let ctor = match ctor # typ with 
		     | [] -> <:ctyp< `$name$ >> 
		     | l -> let l = List.map recurse l in
			    <:ctyp< `$name$ of $list:l$ >>
		   in 
		   <:ctyp< $ctor$ | $acc$ >>

                 end p <:ctyp< >> in 
		 <:ctyp< [ = $l$ ] >>

    | `param (f,t) -> let t = recurse t in
		      let m = in_module f in
		      <:ctyp< $id:m$ $t$ >>

    | `param2 (f,s,t) -> let t = recurse t in
			 let s = strtype s in
			 let m = in_module f in
			 <:ctyp< $id:m$ $s$ $t$ >>
  in

  let dcl = Ast.TyDcl (_loc, "t", [], recurse def, []) in 
  <:str_item< type $dcl$ >>

let generate loc def = 
  generate_type loc def

EXTEND Gram 
  GLOBAL: str_item ;

  str_item: LEVEL "top" [
    [ "type"; LIDENT "json"; loc = [ LIDENT "t" -> _loc ]; "="; def = typedef -> 
      generate loc def ]      
  ];

  name: [ 
    [ s = LIDENT -> (_loc,s) ]
  ];

  string: [ 
    [ s = STRING -> (_loc,Camlp4.Struct.Token.Eval.string ~strict:() s) ] 
  ];

  typedef: [ 
    [ v = variant -> (_loc, `variant v)
    | r = record  -> (_loc, `record  r) 
    | e = typexpr  -> e ]
  ];
  
  variant: [ 
    [ OPT "|"; list = LIST1 variant_bind SEP "|" -> 
      unique list ; list ]
  ];

  of_type: [
    [ "of" ; list = LIST1 typexpr LEVEL "simple" SEP "*" -> list ] 
  ];

  variant_bind: [
    [ name = name ; label = OPT string ; typ = OPT of_type -> 
      make_variant_bind ~name ?label ?typ () ] 
  ];  

  record: [ 
    [ "{"; list = record_members -> unique list ; list ]
  ];
      
  record_members: [ 
    [ m = member_bind ; "}" -> [m]
    | m = member_bind ; ";" ; "}" -> [m] 
    | h = member_bind ; ";" ; t = record_members -> h :: t
    ]
  ];

  default: [
    [ "="; e = expr LEVEL "apply" -> e ] 
  ];

  col_type: [
    [ ":"; t = typexpr -> t ]
  ]; 

  member_bind: [
    [ m = OPT "mutable"; name = name; label = OPT string; typ = col_type ->
      make_member_bind ~mut:false ~name ?label typ
    | m = OPT "mutable"; "?"; name = name; label = OPT string; typ = col_type; def = OPT default -> 
      make_member_bind ~mut:false ~name ?label ~default:def typ ]
  ];

  object_members: [ 
    [ m = member_bind ; ">" -> [m]
    | m = member_bind ; ";" ; ">" -> [m] 
    | h = member_bind ; ";" ; t = object_members -> h :: t
    ]
  ];

  typexpr: [
      "top" [
	h = typexpr; "*"; t = LIST1 typexpr LEVEL "simple" SEP "*" -> 
	(_loc, `tuple (h :: t)) 	
      ]
	
    | "simple" [
      
        LIDENT "string" -> (_loc,`string)
      | LIDENT "bool"   -> (_loc,`bool) 
      | LIDENT "float"  -> (_loc,`float) 
      | LIDENT "int"    -> (_loc,`int)
	
      | t = typexpr ; LIDENT "option" -> (_loc, `option t) 
      | t = typexpr ; LIDENT "list"   -> (_loc, `list t) 
      | t = typexpr ; LIDENT "array"  -> (_loc, `array t) 
      
      | t = typexpr ; u = typemod -> (_loc, `param (u,t))
      
      | "("; s = strtype; ","; t = typexpr; ")"; u = typemod -> (_loc,`param2 (u,s,t))
      
      | "<"; m = object_members  -> (_loc,`obj m) 
      
      | "("; t = typexpr; ")" -> t
      
      | "["; l = poly; "]" -> (_loc,`poly l)     

      | t = typemod -> (_loc,`m t) 
    ]
  ];

  poly_name: [
    [ "`"; name = [ `(LIDENT id|UIDENT id) -> (_loc,id) ] -> name ]
  ];

  poly_bind: [
    [ name = poly_name ; label = OPT string ; typ = OPT of_type -> 
      make_poly_bind ~name ?label ?typ () ] 
  ];

  strpoly_bind: [
    [ name = poly_name ; label = OPT string -> 
      make_strpoly_bind ~name ?label () ]
  ];

  poly: [
    [ list = LIST1 poly_bind SEP "|" -> 
      unique list ; list ]
  ];

  strpoly: [
    [ list = LIST1 strpoly_bind SEP "|" -> 
      unique list ; list ]
  ];

  strtype: [
    [ LIDENT "string" -> (_loc,`string) 
    | t = typemod -> (_loc, `m t) 
    | "["; l = strpoly; "]" -> (_loc, `poly l) ]
  ];

  typemod_sub: [
    [ LIDENT "t" -> []
    | u = [ u = UIDENT -> (_loc,u) ]; "."; s = typemod_sub -> u :: s ] 
  ];

  typemod: [
    [ u = [ u = UIDENT -> (_loc,u) ]; "."; s = typemod_sub -> (_loc, u::s)  ]
  ]; 

END;;
