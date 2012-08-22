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
  symlink (check_plugin plugin) (Filename.concat Path.plugins plugin) 

let plug list = 
  List.iter plug_one list

(* Remove symlinks to unplug modules *)

let unplug_one plugin = 
  let _ = check_plugin plugin in 
  unlink (Filename.concat Path.plugins plugin) 

let unplug list = 
  List.iter unplug_one list
