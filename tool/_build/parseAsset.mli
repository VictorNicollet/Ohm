type token =
  | STYLE of ( string )
  | OPEN_LIST of ( SyntaxAsset.pos )
  | ELSE of ( SyntaxAsset.pos )
  | OPEN_IF of ( SyntaxAsset.pos )
  | OPEN_OPTION of ( SyntaxAsset.pos )
  | DOT of ( SyntaxAsset.pos )
  | OPEN of ( SyntaxAsset.pos )
  | OPEN_SUB of ( SyntaxAsset.pos )
  | CLOSE_SUB of ( SyntaxAsset.pos )
  | OPEN_DEF of ( SyntaxAsset.pos )
  | CLOSE_DEF of ( SyntaxAsset.pos )
  | EOL of ( SyntaxAsset.pos )
  | EQUAL of ( SyntaxAsset.pos )
  | CLOSE of ( SyntaxAsset.pos )
  | PIPE of ( SyntaxAsset.pos )
  | CLOSE_IF of ( SyntaxAsset.pos )
  | CLOSE_LIST of ( SyntaxAsset.pos )
  | CLOSE_OPTION of ( SyntaxAsset.pos )
  | OPEN_SDEF of ( SyntaxAsset.pos )
  | STR of ( string * SyntaxAsset.pos )
  | MODULE of ( string * SyntaxAsset.pos )
  | IDENT of ( string * SyntaxAsset.pos )
  | VARIANT of ( string * SyntaxAsset.pos )
  | ERROR of ( char * SyntaxAsset.pos )
  | EOF

val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  SyntaxAsset.cell list 
