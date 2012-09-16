(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives

open Common 

let publish source destination = 
  
  if not (BatString.starts_with destination "/") then
    error "Invalid destination"
      (Printf.sprintf "Destination path %S should start with a /" destination) ;
  if destination = "/" then
    error "Invalid destination" "For your own sanity, please do not publish to /" ;
  let dest = Filename.concat Path.www (BatString.tail destination 1) in

  let src = if Filename.is_relative source then Filename.concat Path.root source else source in 
  if not (file_exists src) then
    error "Missing source"
      (Printf.sprintf "Source file %S was not found" src) ;

  mkdir_ensure (Filename.dirname dest) 0o755 ;
  
  if copy_if_newer src dest then
    Printf.printf "Publish %s -> %s\n" source destination

let explore () = 

  let rec aux path = 

    let fullpath = Filename.concat Path.public path in
    
    let stat = 
      try Unix.stat fullpath       
      with exn ->      
	path_error "Recursive publishing failure" "Could not stat %S : %s\n" fullpath exn
    in

    match stat.Unix.st_kind with 
      | Unix.S_REG -> publish fullpath ("/" ^ path) 
      | Unix.S_DIR -> 

	let contents = readdir fullpath in
	
	let valid = 
	  List.filter (fun str -> 
	    not (BatString.starts_with str ".")
	    && not (BatString.ends_with str "~") 
	    && not (BatString.starts_with str "#")
	  ) contents
	in
	
	List.iter (Filename.concat path |- aux) valid
	  
      | _ -> ()
	
  in

  aux ""

let process pairs = 
  List.iter (fun (src,dest) -> publish src dest) pairs

let rec parse ?(acc=[]) = function
  | [] -> process acc
  | [x] -> error "Missing destination" 
    (Printf.sprintf "ohm publish does not know what to do with source %S" x)
  | src :: dest :: t -> parse ~acc:((src,dest) :: acc) t

let run = function 
  | []   -> explore () 
  | list -> parse list
