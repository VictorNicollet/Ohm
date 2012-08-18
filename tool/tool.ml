(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives

module Asset = Asset

(* Define all errors that might be encountered --------------------------------------------- *)

let error reason explanation = 
  Printf.printf "%s\n\n%s\n\nPID= %d | UID= %d | eUID= %d | GID= %d | eGID = %d \nCWD= %s\n" 
    reason explanation 
    (Unix.getpid ())
    (Unix.getuid ())
    (Unix.geteuid ())
    (Unix.getgid ())
    (Unix.getegid ())
    (Sys.getcwd ())  ;
  exit (-1)

let path_error title format path exn = 
  error title (Printf.sprintf format path (Printexc.to_string exn))  

let path2_error title format path1 path2 exn = 
  error title (Printf.sprintf format path1 path2 (Printexc.to_string exn))  

let error_mkdir_failure = 
  path_error 
    "Could not create directory."
    "ohm needs to create directory %S, but Unix.mkdir raised an exception: '%s'"
    
let mkdir path access = try Unix.mkdir path access with exn -> error_mkdir_failure path exn

let error_is_dir = 
  path_error 
    "Could not explore directory."
    "ohm needs to make sure directory %S exists, but Sys.is_directory raised an exception: '%s'"
    
let is_dir path = 
  try Sys.is_directory path with 
    | Sys_error str when str = path ^ ": No such file or directory" -> false
    | exn -> error_is_dir path exn 

let error_readdir = 
  path_error 
    "Could not read directory."
    "ohm needs to read the contents of directory %S, but Sys.readdir raised an exception: '%s'"

let readdir path = 
  try Array.to_list (Sys.readdir path) with exn -> error_readdir path exn 

let error_readfile = 
  path_error
    "Could not read file."
     "ohm needs to read the contents of file %S, but open_in_bin raised an exception: '%s'"

let readfile path = 
  try let channel = Pervasives.open_in_bin path in 
      let length  = in_channel_length channel in 
      let string  = String.create length in 
      let ()      = Pervasives.really_input channel string 0 length in 
      Pervasives.close_in channel ;
      string
  with exn -> error_readfile path exn

let readfile_lexbuf path f = 
  try let channel = Pervasives.open_in_bin path in
      let lexbuf  = Lexing.from_channel channel in
      let result  = try Ok (f lexbuf) with exn -> Bad exn in
      Pervasives.close_in channel ;
      result
  with exn -> error_readfile path exn 

let error_writefile = 
  path_error
    "Could not write file."
     "ohm needs to write the contents of file %S, but open_out_bin raised an exception: '%s'"

let putfile path contents = 

  let should = 
    try Digest.file path <> Digest.string contents
    with _ -> true
  in

  if should then begin
    try let channel = Pervasives.open_out_bin path in 
	Pervasives.output_string channel contents ;
	Pervasives.close_out channel 
    with exn -> error_writefile path exn ;
  end ;

  should
      
let error_parse path = function
  | Asset.ParseError pos ->
    error
      "Could not parse file."
      (Printf.sprintf 
	 "%s:%d char %d"
	 path pos.Lexing.pos_lnum Lexing.(pos.pos_cnum - pos.pos_bol))
	 
  | exn ->  
    path_error
      "Could not parse file."
      "ohm tried to parse file %S but encountered an error: '%s'"
      path exn 
      
let system command e = 
  try let () = print_endline command in
      let result = Sys.command command in 
      if result <> 0 then 
	error e
	  (Printf.sprintf "The command %S returned error code %d" command result) 
  with exn ->
    error e
      (Printf.sprintf "The command %S raised exception %s" command (Printexc.to_string exn)) 	  

let lessc from into = 
  system ("lessc -x " ^ Filename.quote from ^ " > " ^ Filename.quote into) 
    "Could not compile LESS CSS sources to final CSS."

let coffeescript from into = 
  system ("coffee -pc " ^ Filename.quote from ^ " > " ^ Filename.quote into) 
    "Could not compile LESS CSS sources to final CSS."

let symlink source dest = 
  try let is_present = try Unix.readlink dest = source with _ -> false in
      if not is_present then Unix.symlink source dest 
  with exn ->
    path2_error 
      "Could not create symlink."
      "ohm tried to create link %S to path %S but Unix.symlink encountered an error: '%s'" 
      source dest exn

(* Find out the root of the current project, and define relevant subdirectories ------------ *)
  
module Path = struct

  let root = Sys.getcwd () 
      
  let () = Unix.chdir root

  let ocaml   = Filename.concat root  "ocaml"
  let plugins = Filename.concat ocaml "plugins"
  let ohm     = Filename.concat ocaml "ohm"
  let www     = Filename.concat root  "www"
  let gen     = Filename.concat ocaml "gen"
  let assets  = Filename.concat root  "assets" 
  let build   = Filename.concat root  "_build"
  let public  = Filename.concat www   "public"

  let less    = Filename.concat build "style.less"
  let css     = Filename.concat build "style.css"
  let css_sym = Filename.concat public "style.css"
  let css_url = "/public/style.css"    

  let coffee  = Filename.concat build "script.coffee"
  let js      = Filename.concat build "script.js"
  let js_sym  = Filename.concat public "script.js"
  let js_url  = "/public/script.js"    

  let assetml = Filename.concat build "asset.ml"

  let jsml    = Filename.concat build "js.ml"
  let jsmli   = Filename.concat build "js.mli"

end

(* Determine what we are expected to do ---------------------------------------------------- *)

let action = ref `LocateAssets 

let () = Arg.parse [

  "locate-assets", Arg.Unit (fun () -> action := `LocateAssets), 
    "Locate and list web application assets." ;

  "assets", Arg.Unit (fun () -> action := `CompileAssets), 
    "Compile all web application assets." ;

  "init", Arg.Unit ignore, 
    "Initialize a new project directory" ;

  "build", Arg.Unit (fun () -> action := `Build),
    "Compile the application code" ;
  
  "full-build", Arg.Unit (fun () -> action := `FullBuild),
    "Equivalent to 'assets' followed by 'build'" ;

] ignore "ohm: perform an operation on your ohm-powered web app."

 
(* Listing assets (a common subroutine) ---------------------------------------------------- *)

type asset = [ `View | `Coffee | `Less | `AdLib ] 

let assets = lazy begin 
  
  let explore path accum =
    List.fold_left begin fun accum item -> 
      let ok kind = (kind, Filename.concat path item) :: accum in 
      if item <> ".htm" && BatString.ends_with item ".htm" then ok `View else
	if item <> ".css" && BatString.ends_with item ".css" then ok `Less else
	  if item <> ".coffee" && BatString.ends_with item ".coffee" then ok `Coffee else
	    if item <> ".adlib.ml" && BatString.ends_with item ".adlib.ml" then ok `AdLib else
	      accum 
    end accum (readdir path)
  in

  let at_root = 
    List.fold_left 
      (fun accum path -> explore (Filename.concat Path.assets path) accum) 
      [] (readdir Path.assets)
  in

  let and_in_plugins = 
    List.fold_left
      (fun accum path -> 
	let plugin = Filename.concat Path.plugins path in
	if is_dir plugin then 
	  let assets = Filename.concat plugin "assets" in
	  if is_dir assets then explore assets accum else accum
	else accum)
      at_root (readdir Path.plugins)
  in
  
  and_in_plugins

end

(* Parsing assets ------------------------------------------------------------------------- *)

let parsed_assets = lazy begin 

  let assets = Lazy.force assets in 
  
  (* Extracting HTML *)

  let parse_html (kind,asset) = 
    match kind with `Coffee | `Less | `AdLib -> None | `View -> 
      let result = readfile_lexbuf asset Asset.parse in
      match result with 
	| Ok stream -> Some (asset,stream) 
	| Bad exn   -> error_parse asset exn 
  in

  let streams = BatList.filter_map parse_html assets in 
  let strings, streams = Asset.extract_strings streams in 

  let html = strings.SyntaxAsset.html in

  let revname_of_path path = 
    let path = BatString.head path (String.length path - String.length ".htm") in
    let segs = BatString.nsplit path "/" in (* This might not work on windows... *)
    let segs = List.filter ( (<>) "" ) segs in 
    let rec extract = function 
      | [ "assets" ; root ; view ] 
      | [ "plugins" ; root ; "assets" ; view ] -> [ view ; root ] 
      | [] -> assert false (* This should never happen *)
      | _ :: t -> extract t 
    in
    extract segs
  in

  let streams = List.map (fun (asset,stream) -> revname_of_path asset, stream) streams in
  let templates = Asset.extract_assets streams in 

  let generated = 
    List.concat
      (Asset.generate_source html
       :: (List.map (fun (revpath,asset) -> Asset.generate_asset revpath asset) templates))
  in

  let generated = 
    List.map (fun (file,asset) -> Filename.concat Path.build file, asset) generated 
  in

  (* Extracting AdLib *)

  let parse_adlib (kind,asset) = 
    match kind with 
      | `Coffee | `Less | `View -> None 
      | `AdLib -> Some (asset, readfile asset) 
  in

  let adlibs = BatList.filter_map parse_adlib assets in
  let generated = 
    (List.map (fun (file,asset) -> Filename.concat Path.build file, asset) 
       (Asset.generate_adlib adlibs))
    @ generated 
  in
      
  (* Extracting LESS *)
  
  let parse_css (kind,asset) =
    match kind with 
      | `Coffee | `View | `AdLib -> None 
      | `Less -> Some (asset, readfile asset)
  in

  let css = BatList.filter_map parse_css assets in
  let css = List.sort (fun a b -> compare (fst a) (fst b)) css in
  
  let all_css = String.concat "\n" (List.map snd css) in
  let all_css = all_css ^ "\n" ^ Buffer.contents strings.SyntaxAsset.css in
  let css_md5 = Digest.to_hex (Digest.string all_css) in

  let generated = (Path.less, all_css) :: generated in

  (* Extracting COFFEE *)

  let parse_coffee (kind,asset) =
    match kind with 
      | `Less | `View | `AdLib -> None
      | `Coffee -> Some (asset, readfile asset)
  in

  let coffee = BatList.filter_map parse_coffee assets in
  let coffee = List.sort (fun a b -> compare (fst a) (fst b)) coffee in
  
  let all_coffee = String.concat "\n" (List.map snd coffee) in
  let all_coffee = all_coffee ^ "\n" ^ Buffer.contents strings.SyntaxAsset.coffee in
  let coffee_md5 = Digest.to_hex (Digest.string all_coffee) in

  let js_ml, js_mli = Coffee.extract_types all_coffee in

  let generated = 
    (Path.jsml, js_ml)
    :: (Path.jsmli, js_mli) 
    :: (Path.coffee, all_coffee)
    :: generated 
  in  

  (* Generating the "assets" file *)

  let assets_file = 
    "(* This file was generated by ohm *)\n"
    ^ (Printf.sprintf "let css = %S\n" (Path.css_url ^ "?" ^ String.sub css_md5 0 8))
    ^ (Printf.sprintf "let js  = %S\n" (Path.js_url  ^ "?" ^ String.sub coffee_md5 0 8))
  in

  let generated = (Path.assetml, assets_file) :: generated in

  (* Generating all files *)

  List.iter (fun (path,contents) -> 
    if putfile path contents then
      print_endline ("Generating " ^ path ^ " ...") ;
  ) generated ;

  if not (is_dir Path.public) then mkdir Path.public 0o751 ;

  (* Compiling LESS CSS and making a public symlink. *)

  lessc Path.less Path.css ;
  symlink Path.css Path.css_sym ;
  
  (* Compiling CoffeeScript and making a public symlink. *)

  coffeescript Path.coffee Path.js ;
  symlink Path.js Path.js_sym ;

end

(* Actions -------------------------------------------------------------------------------- *)

let locateAssets () = 
  List.iter (snd |- print_endline) (Lazy.force assets) 

let compileAssets () = 
  ignore (Lazy.force parsed_assets)

let build () = 
  Sys.chdir Path.ocaml ;
  system (String.concat " " [
    "ocamlbuild" ;
    "-Xs ohm" ;
    "-use-ocamlfind" ;
    "-lib ohm " ;
    "-cflags -I," ^ Filename.quote Path.ohm ^ "," ^ Filename.quote Path.gen ;
    "-lflags -I," ^ Filename.quote Path.ohm ^ "," ^ Filename.quote Path.gen ;
    "main.byte"
  ]) "Could not compile OCaml application" ;
  Sys.chdir Path.root 
    
match !action with 
  | `LocateAssets -> locateAssets () 
  | `CompileAssets -> compileAssets () 
  | `Build -> build () 
  | `FullBuild -> compileAssets () ; build ()

