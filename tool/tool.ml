(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives

module Asset = Asset

(* Define all errors that might be encountered --------------------------------------------- *)

let error reason explanation = 
  Printf.printf "%s\n\n%s\n\nPID= %d | UID= %d | eUID= %d | GID= %d | eGID = %d \nCWD= %s" 
    reason explanation 
    (Unix.getpid ())
    (Unix.getuid ())
    (Unix.geteuid ())
    (Unix.getgid ())
    (Unix.getegid ())
    (Sys.getcwd ())  ;
  exit (-1)

let error_no_project_root () = 
  error 
    "Could not find project root."
    "The project root is the directory that contains subdirectories 'www' and 'ocaml' (among others). ohm-tool should be executed at the project root, but it can sometimes find the project root on its own if executed in one of the project subdirectories." 

let path_error title format path exn = 
  error title (Printf.sprintf format path (Printexc.to_string exn))  

let error_mkdir_failure = 
  path_error 
    "Could not create directory."
    "ohm-tool needs to create directory %S, but Unix.mkdir raised an exception: '%s'"
    
let mkdir path access = try Unix.mkdir path access with exn -> error_mkdir_failure path exn

let error_is_dir = 
  path_error 
    "Could not explore directory."
    "ohm-tool needs to make sure directory %S exists, but Sys.is_directory raised an exception: '%s'"
    
let is_dir path = 
  try Sys.is_directory path with 
    | Sys_error str when str = path ^ ": No such file or directory" -> false
    | exn -> error_is_dir path exn 

let error_readdir = 
  path_error 
    "Could not read directory."
    "ohm-tool needs to read the contents of directory %S, but Sys.readdir raised an exception: '%s'"

let readdir path = 
  try Array.to_list (Sys.readdir path) with exn -> error_readdir path exn 

let error_readfile = 
  path_error
    "Could not read file."
     "ohm-tool needs to read the contents of file %S, but open_in_bin raised an exception: '%s'"

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

let error_parse =
  path_error
    "Could not parse file."
    "ohm-tool tried to parse file %S but encountered an error: '%s'"

(* Find out the root of the current project, and define relevant subdirectories ------------ *)
  
module Path = struct

  let root = 
    let rec find path = 
      if   is_dir (Filename.concat path "www") 
	&& is_dir (Filename.concat path "ocaml") 
      then path 
      else if path = "/" then error_no_project_root () 
      else find (Filename.dirname path) 
    in
    find (Sys.getcwd ())
      
  let () = Unix.chdir root

  let ocaml   = Filename.concat root  "ocaml"
  let plugins = Filename.concat ocaml "plugins"
  let www     = Filename.concat root  "www"
  let assets  = Filename.concat root  "assets" 
  let build   = Filename.concat root  "_build"
    
end

(* Determine what we are expected to do ---------------------------------------------------- *)

let action = ref `LocateAssets 

let () = Arg.parse [

  "-locate-assets", Arg.Unit (fun () -> action := `LocateAssets), 
    "Locate and list web application assets." ;

  "-assets", Arg.Unit (fun () -> action := `CompileAssets), 
    "Compile all web application assets."

] ignore "ohm-tool: perform an operation on your ohm-powered web app."

(* Ensure that the build subdirectory exists and, if it does not, create it ---------------- *)

let () = 
  if not (is_dir Path.build) then mkdir Path.build 0o751
 
(* Listing assets (a common subroutine) ---------------------------------------------------- *)

type asset = [ `View | `Coffee | `Less ] 

let assets = lazy begin 
  
  let explore path accum =
    List.fold_left begin fun accum item -> 
      let ok kind = (kind, Filename.concat path item) :: accum in 
      if item <> ".htm" && BatString.ends_with item ".htm" then ok `View else
	if item <> ".css" && BatString.ends_with item ".css" then ok `Less else
	  if item <> ".coffee" && BatString.ends_with item ".coffee" then ok `Coffee else
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
  
  let parse (kind,asset) = 
    match kind with `Coffee | `Less -> None | `View -> 
      let result = readfile_lexbuf asset Asset.parse in
      match result with 
	| Ok stream -> Some (asset,stream) 
	| Bad exn   -> error_parse asset exn 
  in

  let streams = BatList.filter_map parse assets in 
  let buffer, streams = Asset.extract_strings streams in 

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
  let assets  = Asset.extract_assets streams in 
  
  let generated = 
    List.concat
      (Asset.generate_source buffer
       :: (List.map (fun (revpath,asset) -> Asset.generate_asset revpath asset) assets))
  in
  
  List.iter (fun (file,contents) -> 
    print_endline ("(* == " ^ file ^ " == *)") ;
    print_endline contents
  ) generated ;
  
end

(* Actions -------------------------------------------------------------------------------- *)

let locateAssets () = 
  List.iter (snd |- print_endline) (Lazy.force assets) 

let compileAssets () = 
  ignore (Lazy.force parsed_assets)

match !action with 
  | `LocateAssets -> locateAssets () 
  | `CompileAssets -> compileAssets () 
