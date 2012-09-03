(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives

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

let file_exists path = 
  try Sys.file_exists path with 
    | exn -> path_error "Could not check if file exists."
      "ohm wanted to test if file %S exists, but Sys.file_exists raised an exception: '%s'" 
      path exn 

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

let unlink path = 
  try Unix.unlink path with exn ->
    path_error 
      "Could not unlink."
      "ohm tried to remove link %S but Unix.unlink encountered an error: '%s'"
      path exn 
