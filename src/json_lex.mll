{
  (* Ohm is © 2012 Victor Nicollet *)

  let val_true = Json_type.Bool true
  let val_false = Json_type.Bool false
  let val_empty_array = Json_type.Array []
  let val_empty_object = Json_type.Object []
  let val_empty_string = Json_type.String ""
  let val_zero = Json_type.Int 0
    
  let float f =
    try Json_type.Float (float_of_string f)
    with _ -> raise (Json_type.Error ("Could not parse '" ^ f ^ "' as float"))

  let int i =
    try Json_type.Int (int_of_string i)
    with _ -> try Json_type.Float (float_of_string i)
      with _ -> raise (Json_type.Error ("Could not parse '" ^ i ^ "' as int"))

  let eof = Json_type.Error "Unexpected end of string"
  let unexpected s = Json_type.Error ("Unexpected string '" ^ s ^ "'")
    
  let append buffer lexbuf =
    Buffer.add_substring buffer lexbuf.Lexing.lex_buffer lexbuf.Lexing.lex_start_pos
      (lexbuf.Lexing.lex_curr_pos - lexbuf.Lexing.lex_start_pos)
      
  let add_unicode buffer hex4 =
    let code = int_of_string ("0x"^hex4) in
    if code < 128 then
      Buffer.add_char buffer (char_of_int code)
    else if code < 2048 then
      ( Buffer.add_char buffer (char_of_int (0xc0 + code/64)) ;
	Buffer.add_char buffer (char_of_int (0x80 + code mod 64)) )
    else
      ( Buffer.add_char buffer (char_of_int (0xe0 + code/4096)) ;
	Buffer.add_char buffer (char_of_int (0x80 + (code/64) mod 64)) ;
	Buffer.add_char buffer (char_of_int (0x80 + code mod 64)) )
	
}

let whitespace = [' ' '\n' '\t' '\r']
let digit = ['0' - '9']
let int = '-'? ( '0' | ['1'-'9'] digit* )
let easystring = [^ '"' '\\'] * 
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
 
rule value = parse
   "\xEF\xBB\xBF" { value lexbuf }
 | "null" { Json_type.Null }
 | "true" { val_true } 
 | "false" { val_false } 
 | '"' '"' { val_empty_string }
 | '"' ( easystring as s ) '"' { Json_type.String s }
 | '{' whitespace* '}' { val_empty_object }
 | '[' whitespace* ']' { val_empty_array }
 | whitespace+ { value lexbuf }
 | '-'? '0' { val_zero } 
 | (int '.' digit*) as f { float f } 
 | (int ('.' digit*)? ['e' 'E'] ['-' '+']? digit+) as f { float f } 
 | int as i { int i }   
 | '"' { Json_type.String (string (Buffer.create 16) lexbuf) }
 | '[' { array_end [value lexbuf] lexbuf }
 | '{' { object_colon (key lexbuf) [] lexbuf }
 | eof { raise eof }
     
and array_end list = parse
   whitespace* ']' { Json_type.Array (List.rev list) }
 | whitespace* ',' { array_end (value lexbuf :: list) lexbuf }
 | whitespace* eof { raise eof }

and object_colon key list = parse
   whitespace* ':' { object_end ((key,value lexbuf) :: list) lexbuf }
 | whitespace* eof { raise eof }

and object_end list = parse
   whitespace* '}' { Json_type.Object (List.rev list) }
 | whitespace* ',' { object_colon (key lexbuf) list lexbuf }
 | whitespace* eof { raise eof }
     
and key = parse
   whitespace* '"' ( easystring as s ) '"' { s }
 | whitespace* '"' { string (Buffer.create 16) lexbuf }  
 | whitespace* eof { raise eof }
 
and string buffer = parse
   '"' { Buffer.contents buffer }
 | [^ '"' '\\'] + { append buffer lexbuf ; string buffer lexbuf }  
 | "\\n" { Buffer.add_char buffer '\n' ; string buffer lexbuf } 
 | "\\b" { Buffer.add_char buffer '\b' ; string buffer lexbuf } 
 | "\\t" { Buffer.add_char buffer '\t' ; string buffer lexbuf } 
 | "\\f" { Buffer.add_char buffer (char_of_int 12) ; string buffer lexbuf } 
 | "\\r" { Buffer.add_char buffer '\r' ; string buffer lexbuf }
 | "\\\\" { Buffer.add_char buffer '\\' ; string buffer lexbuf }
 | "\\\"" { Buffer.add_char buffer '\"' ; string buffer lexbuf } 
 | "\\u" { unicode buffer lexbuf } 
 | eof { raise eof } 
 
and unicode buffer = parse
   (hex hex hex hex) as s { add_unicode buffer s ; string buffer lexbuf } 
 | eof { raise eof } 
