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
  
  match Array.to_list Sys.argv with [] -> error "Array.length Sys.argv = 0 ... what the hell ?" | _ :: args -> 
    exit (Sys.command (String.concat " " (List.map Filename.quote (tool :: args))))

let project, name = 
  if Array.length Sys.argv <> 3 || Sys.argv.(1) <> "init" then forward () ;
  let path = Sys.argv.(2) in
  let name = String.uncapitalize (Filename.basename path) in 
  if Filename.is_relative path then 
    Filename.concat (Sys.getcwd ()) path, name 
  else
    path, name 

let path seq = List.fold_left Filename.concat project seq   

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

  let mkexec p = 
    run "chmod u+x %s" (Filename.quote (path p))

end

(* Create the entire directory structure for the Ohm project. These operations
   are all idempotent and respect data that was already created. *)

let () = List.iter Install.mkdir [
  [ "" ] ;
  [ ".ohm" ] ;
  [ "ocaml" ; "plugins" ] ;
  [ "_build" ] ;
  [ "assets" ] ;
  [ "bot" ] ;
  [ "www" ; "public" ]
]

(* Load (or update) the two parts of the Ohm framework (core and plugins) from 
   the github repository. *)

let () = List.iter (fun (path,src) -> Install.clone path src) [
  [ ".ohm" ; "Ohm" ], "git://github.com/VictorNicollet/Ohm.git" ;
  [ ".ohm" ; "Ohm-Plugins" ], "git://github.com/VictorNicollet/Ohm-Plugins.git" ;
]

(* Build the framework *)

let () = Install.run "make --quiet -C %s" (Filename.quote (path [".ohm" ; "Ohm"]))

(* Create the relevant symlinks *)

let () = List.iter (fun (src,dest) -> Install.symlink src dest) [
  [ "ocaml" ; "gen" ], [ "_build" ] ;
  [ "ocaml" ; "ohm" ], [ ".ohm" ; "Ohm" ] ;
]

(* Copy over files *)

let () = List.iter (fun path -> Install.copy ([".ohm";"Ohm";"install"]@path) path) [
  [ "bot" ; "run" ] ;
  [ "Makefile" ] ;
  [ "ocaml" ; "myocamlbuild.ml" ] ;
  [ "ocaml" ; "_tags" ] ;
  [ "ocaml" ; "o.ml" ];
  [ "ocaml" ; "main.ml" ]
]

(* Make files executable when appropriate *)

let () = List.iter Install.mkexec [
  [ "bot" ; "run" ]
]
