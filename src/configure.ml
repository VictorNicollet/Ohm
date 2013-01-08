(* Ohm is © 2011 Victor Nicollet *)

type path = 
  [ `Log
  | `Templates
  | `Resources 
  ]

exception Locked of path * string

(* Find the directory that contains ".ohm" *)    
let root = 
  let is_dir dir = try Sys.is_directory dir with _ -> false in
  let rec find dir = 
    if is_dir (Filename.concat dir ".ohm") then dir 
    else let subdir = Filename.dirname dir in 
	 if subdir = dir then "/"
	 else find subdir
  in 
  find (Sys.getcwd())

let log       = ref (false, "-")
let templates = ref (false, root ^ "/gen/views/")
let resources = ref (false, root ^ "/res/")

let ref_of_path = function
  | `Log       -> log
  | `Templates -> templates
  | `Resources -> resources

let get path = snd ( ! (ref_of_path path) )

let set path value = 
  let ref_of_path = ref_of_path path in 
  let locked, old_value = !ref_of_path in 
  if locked then raise (Locked (path, old_value)) else 
    ref_of_path := (false, value) 

let lock path = 
  let ref_of_path = ref_of_path path in
  let locked, old_value = !ref_of_path in 
  ref_of_path := (true, old_value) ;
  old_value

