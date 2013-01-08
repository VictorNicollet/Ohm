(* Ohm is © 2012 Victor Nicollet *)

let error fmt = Printf.ksprintf (fun s -> print_string "[FAIL] " ; print_endline s ; exit 1) fmt 

let forward () = 
  
  let rec find path = 
    if (try Sys.is_directory (Filename.concat path ".ohm") with _ -> false) 
    then path 
    else if path = "/" then error "No project found ! Possible solution :
ohm init <project-directory>
cd <project-directory>
ohm ..."  
    else find (Filename.dirname path) 
  in
  
  let root = find (Sys.getcwd ()) in
  Sys.chdir root ;
  let tool = List.fold_left Filename.concat root [".ohm";"Ohm";"tool";"tool.byte"] in
  
  match Array.to_list Sys.argv with 
    | [] -> error "Array.length Sys.argv = 0 ... what the hell ?"
    | _ :: args -> 
      let command = String.concat " " (List.map Filename.quote (tool :: args)) in
      exit (Sys.command command)
	
let project, name = 
  if Array.length Sys.argv <> 3 || Sys.argv.(1) <> "init" then forward () ;
  let path = Sys.argv.(2) in
  let cwd  = Sys.getcwd () in
  let path = 
    if path = ".." then Filename.dirname cwd else
      if path = "." then cwd else 
	if Filename.is_relative path then Filename.concat cwd path else path 
  in 
  let name = String.uncapitalize (Filename.basename path) in 
  path, name

let path seq = List.fold_left Filename.concat project seq   

let readdir p =
  let path = path p in 
  try Array.to_list (Sys.readdir path) 
  with exn -> error "Could not read directory %S : %s" path (Printexc.to_string exn)

module Install = struct

  let prefix = ref "" 

  let in_dir p f = 
    let old_path   = Sys.getcwd () in
    let old_prefix = !prefix in 
    Sys.chdir (path p) ;
    prefix := (!prefix) ^ "  " ;
    Printf.printf "%s>> in %s :\n" old_prefix (path p) ;
    ( try f () with _ -> () ) ;
    Sys.chdir old_path ;
    prefix := old_prefix 

  let run fmt = 
    Printf.ksprintf (fun command -> 
      print_string (!prefix) ; 
      print_endline command ;
      let code = Sys.command command in
      if code <> 0 then error "%S returned error code %d" command code
    ) fmt 

  let mkdir p = 
    let _ = 
      List.fold_left (fun current seg ->
	let p = current @ [seg] in 
	try 
	  if Sys.is_directory (path p) then p else  
	    error "Expected `%s` to be a directory !" (path p)
	with _ ->
	  run "mkdir %s" (Filename.quote (path p)) ; 
	  p 
      ) [] p   
    in ()

  let clone p src = 
    let exists = try Sys.is_directory (path (p @ [".git"])) with _ -> false in 
    if exists then
      in_dir p (fun () -> run "git pull --quiet") 
    else
      run "git clone --quiet %s %s" (Filename.quote src) (Filename.quote (path p))

  let symlink src dest = 
    if not (Sys.file_exists (path src)) then
      run "ln -s %s %s" (Filename.quote (path dest)) (Filename.quote (path src))

  let copy src dest = 
    if not (Sys.file_exists (path dest)) then (
      run "cp %s %s" (Filename.quote (path src)) (Filename.quote (path dest))
    )

  let config () = 
    let path = path [ "ocaml" ; "configProject.ml" ] in
    if not (Sys.file_exists path) then
      try let chan = open_out path in
	  try let name = Printf.sprintf "let name = %S\nlet lname = %S\n"
		name (String.lowercase name) in
	      output_string chan name ;
	      close_out chan
	  with _ -> close_out chan
      with _ -> error "Could not write project config file %s" path 

  let make fresh = 
    in_dir [] (fun () -> 
      if not fresh then run "make clean" ; 
      run "make" 
    )

  let touch p =
    run "touch %s" (Filename.quote (path p))

  let make_plugin plugin = 
    let p = [ ".ohm" ; "Ohm-Plugins" ; plugin ; "tool" ] in
    let exists = try Sys.is_directory (path p) with _ -> false in 
    if exists then 
      run "make -C %s" (Filename.quote (path p))

end

(* Fresh install means there have been no directories or files
   created so far. Just check whether the .install file has been created. *)

let fresh = not (Sys.file_exists (path [".install"]))

(* Create the entire directory structure for the Ohm project. These operations
   are all idempotent and respect data that was already created. *)

let () = List.iter Install.mkdir [
  [ "" ] ;
  [ ".ohm" ] ;
  [ "ocaml" ; "plugins" ] ;
  [ "_build" ] ;
  [ "www" ] ;
  [ "public" ]
]

let () = if fresh then List.iter Install.mkdir [
  [ "assets" ; "common" ] ;
  [ "assets" ; "errorPage" ];
] 

(* Load (or update) the two parts of the Ohm framework (core and plugins) from 
   the github repository. *)

let () = List.iter (fun (path,src) -> Install.clone path src) [
  [ ".ohm" ; "Ohm" ], "git://github.com/VictorNicollet/Ohm.git" ;
  [ ".ohm" ; "Ohm-Plugins" ], "git://github.com/VictorNicollet/Ohm-Plugins.git" ;
]

(* Build the framework *)

let () = Install.run "make --quiet -C %s" (Filename.quote (path [".ohm" ; "Ohm"]))

(* Build the plugins that need building. *)

let () = List.iter Install.make_plugin (readdir [".ohm" ; "Ohm-Plugins"])

(* Create the relevant symlinks *)

let () = List.iter (fun (src,dest) -> Install.symlink src dest) [
  [ "ocaml" ; "gen" ], [ "_build" ] ;
  [ "ocaml" ; "ohm" ], [ ".ohm" ; "Ohm" ] ;
]

(* Copy over files *)

let () = if fresh then List.iter 
    (fun path -> Install.copy ([".ohm";"Ohm";"install"]@path) path) [
      [ "Makefile" ] ;
      [ "ocaml" ; "myocamlbuild.ml" ] ;
      [ "ocaml" ; "_tags" ] ;
      [ "ocaml" ; "o.ml" ];
      [ "ocaml" ; "main.ml" ] ;
      [ "ocaml" ; "cErrorPage.mli" ] ;
      [ "ocaml" ; "cErrorPage.ml" ] ;
      [ "ocaml" ; "configProject.mli" ] ;
      [ "assets" ; "common" ; "def.adlib.ml" ] ;
      [ "assets" ; "common" ; "en.adlib.ml" ] ;
      [ "assets" ; "common" ; "style.css" ] ;
      [ "assets" ; "errorPage" ; "error404.htm" ] ;
      [ "assets" ; "errorPage" ; "style.css" ] ;
      [ "assets" ; "errorPage" ; "def.adlib.ml" ] ;
      [ "assets" ; "errorPage" ; "en.adlib.ml" ] ;
      [ "public" ; "500.htm" ] 
    ]
    
(* Install the configuration file *)

let () = if fresh then Install.config ()

(* Touch the "installed" file to avoid fresh installs from now on. *)

let () = if fresh then Install.touch [".install"]

(* Finish install by compiling the software. *)
  
let () = Install.make fresh

(* Display a nice welcome message. *)

let () = if fresh then List.iter print_endline [
  "================================================================" ;
  "" ;
  "  Welcome to the Ohm Framework !" ;
  "" ;
  "Your project is now ready. A few things you might want to do" ;
  "first:" ;
  "" ;
  "## Add the basic files to a Git repository" ;
  "git add ocaml/*.ml ocaml/_tags assets public bot Makefile .install" ;
  "" ;
  "## Register your project with Apache 2" ;
  "ohm fastcgi apache" ; 
  "sudo ohm vhost apache > /etc/apache/sites-available/" ^ name ^ ".local";
  "sudo a2ensite " ^ name ^ ".local" ;
  "sudo service apache2 reload" ;
  "" ;
  "## Make '" ^ name ^ ".local' point at your project" ;
  "sudo echo '127.0.0.1 " ^ name ^ ".local' >> /etc/hosts" ;
  "" ;
  "Thank you for using Ohm. Enjoy your project!" ;
  "" ;
  "                                        Victor Nicollet"
]
