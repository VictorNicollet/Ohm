(* Ohm is © 2013 Victor Nicollet *)

open Common

let bin_server () = Filename.concat Path.root "server"
let www_server () = Filename.concat Path.www  "server"
let www_socket () = Filename.concat Path.www  "socket" 
let pidfile    () = Filename.concat Path.root "fastcgi.pid" 

let stop ~apache_too = 
  let bin_server = bin_server () in
  let www_server = www_server () in
  let www_socket = www_socket () in
  let pidfile    = pidfile () in

  (* Removing the "server" symlink *)
  let remove = try Unix.readlink www_server = bin_server && apache_too with _ -> false in
  if remove then 
    unlink www_server ;

  (* Killing the FastCGI server process *)
  if file_exists pidfile then begin
    let pid = try int_of_string (readfile pidfile) 
      with exn -> path_error "Invalid PID file."
	"File %s should contain a PID, but parsing failed with exception: '%s'" pidfile exn 
    in 
    system (Printf.sprintf "kill %d" pid) 
      "Killing running FastCGI process" ;
    unlink pidfile 
  end ;

  (* Removing the socket *)
  if file_exists www_socket then 
    unlink www_socket 

let start () = 
  let bin_server = bin_server () in
  let www_socket = www_socket () in
  let pidfile    = pidfile () in
  stop ~apache_too:true ;
  system (Printf.sprintf "spawn-fcgi -P %s -s %s -f %s" pidfile www_socket bin_server)
    "Spawning a FastCGI process" ;
  system (Printf.sprintf "chmod a+w %s" www_socket) 
    "Enable web server to write to FastCGI socket" 

let apache () = 
  let bin_server = bin_server () in
  let www_server = www_server () in 
  stop ~apache_too:false ; 
  symlink bin_server www_server

let rec tool = function 
  | "start" :: _ -> start () 
  | ["stop"] -> stop ~apache_too:true
  | "stop" :: args -> stop ~apache_too:true ; tool args 
  | "apache" :: _ -> apache () 
  | _ -> error "Unrecognized FastCGI control instruction"
    "Expected 'ohm fastcgi start', 'ohm fastcgi stop' or 'ohm fastcgi apache'"

