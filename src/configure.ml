(* Ohm is © 2011 Victor Nicollet *)

type path = 
  [ `Log
  | `Templates
  | `Resources 
  ]

exception Locked of path * string

(* From whatever/build/app to whatever/ *)
let root = Filename.dirname (Filename.dirname Sys.executable_name)

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

