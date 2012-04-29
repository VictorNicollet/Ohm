(* Ohm is © 2012 Victor Nicollet *)

type pos = Lexing.position * Lexing.position 

type cell = 
  | Cell_String of string
  | Cell_Print  of expr 
  | Cell_If     of expr * cell list * cell list
  | Cell_Option of located option * expr * cell list * cell list
  | Cell_List   of located option * expr * cell list * cell list
  | Cell_Sub    of expr * cell list
  | Cell_Define of located * cell list
and expr = located * expr_flag list 
and expr_flag = located list
and located = {
  contents : string ;
  position : pos
}
    
let pos lexbuf = 
  Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf
    
let cell c = c
  
let located (contents,position) = { contents ; position }

(* This cleans up the separate "string" cells by merging them together and applying
   a whitespace-to-' ', ' '*-to-' ', &foo;-to-UTF8 series of transforms that help
   shorten the source. *)
let whitespace_chars =
  String.concat ""
    (List.map (String.make 1)
       [
         Char.chr 9;  (* HT *)
         Char.chr 10; (* LF *)
         Char.chr 11; (* VT *)
         Char.chr 12; (* FF *)
         Char.chr 13; (* CR *)
         Char.chr 32; (* space *)
       ])

let whitespace_re = Str.regexp ("[" ^ whitespace_chars ^ "]+")
let entity_re = Str.regexp "&\\([a-zA-Z0-9]+\\);"  

let clean_string s = 
  let s = Str.global_replace whitespace_re " " s in
  let s = Str.global_substitute entity_re (fun s -> 
    match Str.matched_group 1 s with 
      | "iexcl"  -> "¡"
      | "cent"   -> "¢"
      | "pound"  -> "£"
      | "curren" -> "¤"
      | "yen"    -> "¥"
      | "brvbar" -> "¦"
      | "sect"   -> "§"
      | "uml"    -> "¨"
      | "copy"   -> "©"
      | "ordf"   -> "ª"
      | "laquo"  -> "«"
      | "not"    -> "¬"
      | "reg"    -> "®"
      | "macr"   -> "¯"
      | "deg"    -> "°"
      | "plusmn" -> "±"
      | "sup2"   -> "²"
      | "sup3"   -> "³"
      | "acute"  -> "´"
      | "micro"  -> "µ"
      | "para"   -> "¶"
      | "middot" -> "·"
      | "cedil"  -> "¸"
      | "sup1"   -> "¹"
      | "ordm"   -> "º"
      | "raquo"  -> "»"
      | "frac14" -> "¼"
      | "frac12" -> "½"
      | "frac34" -> "¾"
      | "iquest" -> "¿"
      | "Agrave" -> "À"
      | "Aacute" -> "Á"
      | "Acirc"  -> "Â"
      | "Atilde" -> "Ã"
      | "Auml"   -> "Ä"
      | "Aring"  -> "Å"
      | "AElig"  -> "Æ"
      | "Ccedil" -> "Ç"
      | "Egrave" -> "È"
      | "Eacute" -> "É"
      | "Ecirc"  -> "Ê"
      | "Euml"   -> "Ë"
      | "Igrave" -> "Ì"
      | "Iacute" -> "Í"
      | "Icirc"  -> "Î"
      | "Iuml"   -> "Ï"
      | "ETH"    -> "Ð"
      | "Ntilde" -> "Ñ"
      | "Ograve" -> "Ò"
      | "Oacute" -> "Ó"
      | "Ocirc"  -> "Ô"
      | "Otilde" -> "Õ"
      | "Ouml"   -> "Ö"
      | "times"  -> "×"
      | "Oslash" -> "Ø"
      | "Ugrave" -> "Ù"
      | "Uacute" -> "Ú"
      | "Ucirc"  -> "Û"
      | "Uuml"   -> "Ü"
      | "Yacute" -> "Ý"
      | "THORN"  -> "Þ"
      | "szlig"  -> "ß"
      | "agrave" -> "à"
      | "aacute" -> "á"
      | "acirc"  -> "â"
      | "atilde" -> "ã"
      | "auml"   -> "ä"
      | "aring"  -> "å"
      | "aelig"  -> "æ"
      | "ccedil" -> "ç"
      | "egrave" -> "è"
      | "eacute" -> "é"
      | "ecirc"  -> "ê"
      | "euml"   -> "ë"
      | "igrave" -> "ì"
      | "iacute" -> "í"
      | "icirc"  -> "î"
      | "iuml"   -> "ï"
      | "eth"    -> "ð"
      | "ntilde" -> "ñ"
      | "ograve" -> "ò"
      | "oacute" -> "ó"
      | "ocirc"  -> "ô"
      | "otilde" -> "õ"
      | "ouml"   -> "ö"
      | "divide" -> "÷"
      | "oslash" -> "ø"
      | "ugrave" -> "ù"
      | "uacute" -> "ú"
      | "ucirc"  -> "û"
      | "uuml"   -> "ü"
      | "yacute" -> "ý"
      | "thorn"  -> "þ"
      | "yuml"   -> "ÿ"
      | "OElig"  -> "Œ"
      | "oelig"  -> "œ"
      | "Scaron" -> "Š"
      | "scaron" -> "š"
      | "Yuml"   -> "Ÿ"
      | "fnof"   -> "ƒ"
      | "circ"   -> "ˆ"
      | "tilde"  -> "˜"
      | "Alpha"  -> "Α"
      | "Beta"   -> "Β"
      | "Gamma"  -> "Γ"
      | "Delta"  -> "Δ"
      | "Epsilon" -> "Ε"
      | "Zeta"   -> "Ζ"
      | "Eta"    -> "Η"
      | "Theta"  -> "Θ"
      | "Iota"   -> "Ι"
      | "Kappa"  -> "Κ"
      | "Lambda" -> "Λ"
      | "Mu"     -> "Μ"
      | "Nu"     -> "Ν"
      | "Xi"     -> "Ξ"
      | "Omicron" -> "Ο"
      | "Pi"     -> "Π"
      | "Rho"    -> "Ρ"
      | "Sigma"  -> "Σ"
      | "Tau"    -> "Τ"
      | "Upsilon" -> "Υ"
      | "Phi"    -> "Φ"
      | "Chi"    -> "Χ"
      | "Psi"    -> "Ψ"
      | "Omega"  -> "Ω"
      | "alpha"  -> "α"
      | "beta"   -> "β"
      | "gamma"  -> "γ"
      | "delta"  -> "δ"
      | "epsilon" -> "ε"
      | "zeta"   -> "ζ"
      | "eta"    -> "η"
      | "theta"  -> "θ"
      | "iota"   -> "ι"
      | "kappa"  -> "κ"
      | "lambda" -> "λ"
      | "mu"     -> "μ"
      | "nu"     -> "ν"
      | "xi"     -> "ξ"
      | "omicron" -> "ο"
      | "pi"     -> "π"
      | "rho"    -> "ρ"
      | "sigmaf" -> "ς"
      | "sigma"  -> "σ"
      | "tau"    -> "τ"
      | "upsilon" -> "υ"
      | "phi"    -> "φ"
      | "chi"    -> "χ"
      | "psi"    -> "ψ"
      | "omega"  -> "ω"
      | "thetasym" -> "ϑ"
      | "upsih"  -> "ϒ"
      | "piv"    -> "ϖ"
      | "ensp"   -> " "
      | "emsp"   -> " "
      | "thinsp" -> " "
      | "ndash"  -> "–"
      | "mdash"  -> "—"
      | "lsquo"  -> "‘"
      | "rsquo"  -> "’"
      | "sbquo"  -> "‚"
      | "ldquo"  -> "“"
      | "rdquo"  -> "”"
      | "bdquo"  -> "„"
      | "dagger" -> "†"
      | "Dagger" -> "‡"
      | "bull"   -> "•"
      | "hellip" -> "…"
      | "permil" -> "‰"
      | "prime"  -> "′"
      | "Prime"  -> "″"
      | "lsaquo" -> "‹"
      | "rsaquo" -> "›"
      | "oline"  -> "‾"
      | "frasl"  -> "⁄"
      | "euro"   -> "€"
      | "image"  -> "ℑ"
      | "weierp" -> "℘"
      | "real"   -> "ℜ"
      | "trade"  -> "™"
      | "alefsym" -> "ℵ"
      | "larr"   -> "←"
      | "uarr"   -> "↑"
      | "rarr"   -> "→"
      | "darr"   -> "↓"
      | "harr"   -> "↔"
      | "crarr"  -> "↵"
      | "lArr"   -> "⇐"
      | "uArr"   -> "⇑"
      | "rArr"   -> "⇒"
      | "dArr"   -> "⇓"
      | "hArr"   -> "⇔"
      | "forall" -> "∀"
      | "part"   -> "∂"
      | "exist"  -> "∃"
      | "empty"  -> "∅"
      | "nabla"  -> "∇"
      | "isin"   -> "∈"
      | "notin"  -> "∉"
      | "ni"     -> "∋"
      | "prod"   -> "∏"
      | "sum"    -> "∑"
      | "minus"  -> "−"
      | "lowast" -> "∗"
      | "radic"  -> "√"
      | "prop"   -> "∝"
      | "infin"  -> "∞"
      | "ang"    -> "∠"
      | "and"    -> "∧"
      | "or"     -> "∨"
      | "cap"    -> "∩"
      | "cup"    -> "∪"
      | "int"    -> "∫"
      | "there4" -> "∴"
      | "sim"    -> "∼"
      | "cong"   -> "≅"
      | "asymp"  -> "≈"
      | "ne"     -> "≠"
      | "equiv"  -> "≡"
      | "le"     -> "≤"
      | "ge"     -> "≥"
      | "sub"    -> "⊂"
      | "sup"    -> "⊃"
      | "nsub"   -> "⊄"
      | "sube"   -> "⊆"
      | "supe"   -> "⊇"
      | "oplus"  -> "⊕"
      | "otimes" -> "⊗"
      | "perp"   -> "⊥"
      | "sdot"   -> "⋅"
      | "lceil"  -> "⌈"
      | "rceil"  -> "⌉"
      | "lfloor" -> "⌊"
      | "rfloor" -> "⌋"
      | "lang"   -> "〈"
      | "rang"   -> "〉"
      | "loz"    -> "◊"
      | "spades" -> "♠"
      | "clubs"  -> "♣"
      | "hearts" -> "♥"
      | "diams"  -> "♦"
      | other -> "&"^other^";"
  ) s in
  s

let rec clean_strings = function 
  | [] -> [] 
  | Cell_String a :: Cell_String b :: tail -> clean_strings (Cell_String (a ^ b) :: tail) 
  | Cell_String a :: tail -> Cell_String (clean_string a) :: clean_strings tail
  | Cell_Print  x :: tail -> Cell_Print x :: clean_strings tail
  | Cell_If (e,a,b) :: tail -> Cell_If (e,
					clean_strings a, 
					clean_strings b) :: clean_strings tail 
  | Cell_Option (l,e,a,b) :: tail -> Cell_Option (l,e, 
						  clean_strings a,
						  clean_strings b) :: clean_strings tail
  | Cell_List (l,e,a,b) :: tail -> Cell_List (l,e,
					      clean_strings a,
					      clean_strings b) :: clean_strings tail
  | Cell_Sub (e,l) :: tail -> Cell_Sub (e,clean_strings l) :: clean_strings tail
  | Cell_Define (n,l) :: tail -> Cell_Define (n,clean_strings l) :: clean_strings tail

(* This extracts the strings from the asset AST into a side buffer ("current") 
   by eliminating duplicate strings. This turns every Cell_String into a 
   `String (start,length) that references this side buffer *)

type buffered_cell = 
    [ `Print  of expr
    | `If     of expr * buffered_cell list * buffered_cell list
    | `Option of located option * expr * buffered_cell list * buffered_cell list
    | `List   of located option * expr * buffered_cell list * buffered_cell list
    | `String of int * int
    | `Sub    of expr * buffered_cell list 
    | `Define of located * buffered_cell list
    ]
  
let rec extract_strings current list = 

  let list = clean_strings list in 

  let find string substring = 
    let n = String.length string and m = String.length substring in 
    let rec at i j = i + j >= n || j >= m || string.[i+j] = substring.[j] && at i (succ j) in 
    let rec search i = if at i 0 then i else search (succ i) in
    let start  = search 0 in
    let concat = max 0 (start + m - n) in
    (if concat = 0 then string else string ^ String.sub substring (m - concat) concat),
    start, m 
  in

  let extract current = function
    | Cell_Print e -> current, `Print e
    | Cell_If (e,a,b) -> let current, a = extract_strings current a in
			 let current, b = extract_strings current b in 
			 current, `If (e,a,b) 
    | Cell_Option (l,e,a,b) -> let current, a = extract_strings current a in
			       let current, b = extract_strings current b in
			       current, `Option (l,e,a,b) 
    | Cell_List (l,e,a,b) -> let current, a = extract_strings current a in
			     let current, b = extract_strings current b in 
			     current, `List (l,e,a,b) 
    | Cell_Sub (e,l) -> let current, l = extract_strings current l in
			current, `Sub (e,l) 
    | Cell_Define (n,l) -> let current, l = extract_strings current l in 
			   current, `Define (n,l)
    | Cell_String s -> let current, start, length = find current s in 
		       current, `String (start, length) 
  in

  List.fold_right 
    (fun cell (current, out) -> 
      let current, cell = extract current cell in 
      (current, cell :: out))
    list (current,[])

(* This extracts `Define cells and replaces them with an appropriate `Call cell. The
   extracted definitions all have a complete REVERSED name. *)

type clean_cell = 
  [ `Print  of expr
  | `If     of expr * clean_cell list * clean_cell list
  | `Option of located option * expr * clean_cell list * clean_cell list
  | `List   of located option * expr * clean_cell list * clean_cell list
  | `String of int * int
  | `Sub    of expr * clean_cell list 
  | `Call   of string list
  ]

let rec extract_assets revpath sub (list : buffered_cell list) = 
  let extract sub = function
    | `Print e    -> sub, `Print e 
    | `If (e,a,b) -> let sub, a = extract_assets revpath sub a in
		     let sub, b = extract_assets revpath sub b in 
		     sub, `If (e,a,b) 
    | `Option (l,e,a,b) -> let sub, a = extract_assets revpath sub a in
			   let sub, b = extract_assets revpath sub b in 
			   sub, `Option (l,e,a,b) 
    | `List (l,e,a,b) -> let sub, a = extract_assets revpath sub a in
			 let sub, b = extract_assets revpath sub b in 
			 sub, `List (l,e,a,b) 
    | `Sub (e,l) -> let sub, l = extract_assets revpath sub l in
		    sub, `Sub (e,l)
    | `String (s,l) -> sub, `String (s,l) 
    | `Define (n,l) -> let revpath = n.contents :: revpath in 
		       let sub, l = extract_assets revpath sub l in
		       (revpath, l) :: sub, `Call revpath 
  in
  List.fold_right 
    (fun cell (sub,out) -> 
      let sub, cell = extract sub cell in 
      (sub, cell :: out))
    list (sub,[])

(* This extracts expressions from a cell list upward to a root,
   so they are evaluated together. *)

type rooted_cell = 
    [ `Print  of int 
    | `String of int * int
    ]

and cell_root = 
  [ `Render  of rooted_cell list 
  | `Extract of int * located * cell_root
  | `Apply   of int * int * located list * cell_root
  | `Ohm     of int * int * cell_root
  | `Put     of int * int * [ `Raw | `Esc ] * cell_root
  | `If      of int * int * cell_root * cell_root * cell_root
  | `Option  of int * located option * int * cell_root * cell_root * cell_root
  | `List    of int * located option * int * cell_root * cell_root * cell_root
  | `Sub     of int * int * cell_root * cell_root
  | `Call    of int * string list * cell_root
  ]

let contents x = x.contents

let uid = ref 0
let getuid () = incr uid ; !uid 

let rec extract_roots ?(accum=[]) (list:clean_cell list) = 

  let split_expr ?(printed=false) (var,flags) = 
    let uid  = getuid () in 
    let fill inner = `Extract (uid, var, inner) in
    if printed && flags = [] then
      let uid' = getuid () in
      (uid', fun inner -> fill (`Put (uid',uid,`Esc,inner)))
    else
      List.fold_left begin fun (uid,fill) flag -> 
	match flag with 
	  | [ { contents = "ohm" } ] -> let uid' = getuid () in
					(uid', fun inner -> fill (`Ohm (uid',uid,inner)))
	  | [ { contents = "raw" } ] -> let uid' = getuid () in
					(uid', fun inner -> fill (`Put (uid',uid,`Raw,inner)))
	  | [ { contents = "esc" } ] -> let uid' = getuid () in
					(uid', fun inner -> fill (`Put (uid',uid,`Esc,inner)))
	  | [ { contents = "verbatim" } ] -> uid, fill
	  | [] -> uid, fill
	  | list -> let uid' = getuid () in
		    (uid', fun inner -> fill (`Apply (uid', uid, list, inner)))
      end (uid,fill) flags 
  in

  match list with 
    | [] -> `Render (List.rev accum) 
    | `String (start,length) :: tail -> let accum = `String (start,length) :: accum in 
					extract_roots ~accum tail 
    | `Print expr :: tail -> let uid, fill = split_expr ~printed:true expr in 
			     let accum = `Print uid :: accum in
			     fill (extract_roots ~accum tail) 
    | `Sub (e,l) :: tail -> let uid, fill = split_expr e in 
			    let uid' = getuid () in
			    let fill inner = fill (`Sub (uid', uid, extract_roots l, inner)) in
			    let accum = `Print uid' :: accum in
			    fill (extract_roots ~accum tail) 
    | `Option (l,e,a,b) :: tail -> let uid, fill = split_expr e in
				   let uid' = getuid () in
				   let a, b = extract_roots a, extract_roots b in
				   let fill inner = fill (`Option (uid',l,uid,a,b,inner)) in 
				   let accum = `Print uid' :: accum in 
				   fill (extract_roots ~accum tail) 
    | `List (l,e,a,b) :: tail ->  let uid, fill = split_expr e in
				  let uid' = getuid () in
				  let a, b = extract_roots a, extract_roots b in 
				  let fill inner = fill (`List (uid',l,uid,a,b,inner)) in
				  let accum = `Print uid' :: accum in 
				  fill (extract_roots ~accum tail) 
    | `If (e,a,b) :: tail -> let uid, fill = split_expr e in
			     let uid' = getuid () in
			     let a, b = extract_roots a, extract_roots b in 
			     let fill inner = fill (`If (uid',uid,a,b,inner)) in
			     let accum = `Print uid' :: accum in 
			     fill (extract_roots ~accum tail) 
    | `Call l :: tail -> let uid = getuid () in
			 `Call (uid, l, extract_roots ~accum:(`Print uid :: accum) tail) 
      
