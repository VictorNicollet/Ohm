(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives

open Common 

(* Check whether a plugin exists and is valid. *)

let check_plugin plugin = 
  if Filename.basename plugin <> plugin then 
    error "Invalid plugin name"
      "A plugin name may not contain '/' characters"
  else
    let path = Filename.concat Path.plugsrc plugin in 
    if not (is_dir path) then
      error "Could not find plugin"
	(Printf.sprintf 
	   "ohm expected to find the plugin in directory %s, but that directory does not exist."
	   path) 
    else
      path

(* Symlink plugged-in modules *)

let plug_one plugin =   
  let path = check_plugin plugin in 
  symlink path (Filename.concat Path.plugins plugin) ;
  symlink (Filename.concat path "www") (Filename.concat Path.www plugin) 

let plug list = 
  List.iter plug_one list

(* Remove symlinks to unplug modules *)

let unplug_one plugin = 
  let _ = check_plugin plugin in 
  unlink (Filename.concat Path.plugins plugin) ;
  unlink (Filename.concat Path.www plugin) 

let unplug list = 
  List.iter unplug_one list

(* Run a single plugin, with arguments. *)

let run plugin args = 
  let path = check_plugin plugin in 
  let comm = String.concat " "
    (List.map Filename.quote (Filename.concat path "tool/run" :: args))
  in
  system comm (Printf.sprintf "Could not run plugin tool '%s'" plugin)

let parserun plugin args = 
  let name = BatString.tail plugin (String.length "plugins.") in
  run name args

(* Run all plugins that have a tool enabled, in DWIM mode. *)

let run_all () = 
  let plugins = readdir Path.plugins in
  let tools = List.filter (fun plugin -> 
    file_exists Filename.(concat (concat Path.plugins plugin) "tool/run")
  ) plugins in 
  List.iter (fun plugin -> run plugin ["dwim"]) tools
