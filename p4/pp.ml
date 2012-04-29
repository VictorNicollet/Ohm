open Camlp4.PreCast
open Syntax

EXTEND Gram
    GLOBAL: expr;

    expr: LEVEL "top"
    [
      [ "let"; "!"; p = patt ; "=" ; e = expr ; "in" ; e' = expr ->
        <:expr< (bind ($e$) (fun $p$ -> $e'$)) >> ] 
    ] ;

    expr: LEVEL "simple" 
    [
      [ "#" ; i = LIDENT ->
        <:expr< (fun __obj -> __obj # $i$) >> ]  
    ] ;

END;
