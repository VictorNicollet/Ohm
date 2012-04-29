{ 

  (* Ohm is © 2012 Victor Nicollet *)
  open ParseAsset
  open SyntaxAsset

}

rule outer = parse
  | ( [ ^ '\n' '{' ] | "\\{" ) + as str { STR (str, pos lexbuf)  } 
  | '\n' { Lexing.new_line lexbuf ; EOL (pos lexbuf) } 

  | "{#"     { OPEN_LIST    (pos lexbuf) }
  | "{/#}"   { CLOSE_LIST   (pos lexbuf) } 
  | "{/?}"   { CLOSE_OPTION (pos lexbuf) } 
  | "{else}" { ELSE         (pos lexbuf) } 
  | "{if"    { OPEN_IF      (pos lexbuf) } 
  | "{/if}"  { CLOSE_IF     (pos lexbuf) }
  | "{?"     { OPEN_OPTION  (pos lexbuf) } 
  | "{"      { OPEN         (pos lexbuf) } 
  | "{="     { OPEN_SUB     (pos lexbuf) } 
  | "{/=}"   { CLOSE_SUB    (pos lexbuf) } 
  | "{@"     { OPEN_DEF     (pos lexbuf) }
  | "{/@}"   { CLOSE_DEF    (pos lexbuf) } 

  | eof      { EOF } 

and inner = parse 
  | '\n' { Lexing.new_line lexbuf ; inner lexbuf } 
  | [ ' ' '\t' '\r' ] { inner lexbuf } 
  | [ 'A' - 'Z'] [ 'A'-'Z' 'a'-'z' '_' '0'-'9' ] * as str
      { MODULE (str, pos lexbuf) } 
  | '.' { DOT (pos lexbuf) }  
  | '|' { PIPE (pos lexbuf) } 
  | [ 'a' - 'z' ] [ 'A'-'Z' 'a'-'z' '_' '0'-'9' ] * as str 
      { IDENT (str, pos lexbuf) } 
  | '}' { CLOSE (pos lexbuf) } 
  | '=' { EQUAL (pos lexbuf) } 

  | _ as c { ERROR (c, pos lexbuf) } 
  | eof    { EOF } 

{

  let opens = function 
    | OPEN_LIST _ 
    | OPEN_IF _ 
    | OPEN _ 
    | OPEN_SUB _ 
    | OPEN_DEF _ -> true
    | _ -> false

  let closes = function 
    | CLOSE _ -> true
    | _ -> false

  let read () = 
    let mode = ref `OUTER in 
    fun lexbuf -> 
      match !mode with 
	| `OUTER -> let tok = outer lexbuf in 
		    if opens tok then mode := `INNER ;
		    tok
	| `INNER -> let tok = inner lexbuf in 
		    if closes tok then mode := `OUTER ;
		    tok 

}
