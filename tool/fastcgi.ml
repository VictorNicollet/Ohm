(* Ohm is © 2013 Victor Nicollet *)

open Common

let bin_server () = Filename.concat Path.root "server"
let www_server () = Filename.concat Path.www  "server"
let www_socket () = Filename.concat Path.www  "socket" 
let fastcgi    () = Filename.concat Path.root ".fastcgi"

let stop ~apache_too = 
  let bin_server = bin_server () in
  let www_server = www_server () in
  let www_socket = www_socket () in
  let fastcgi    = fastcgi () in

  (* Removing the "server" symlink *)
  if apache_too then 
    let remove = try Unix.readlink www_server = bin_server with _ -> false in
    if remove then 
      unlink www_server ;

  (* Stopping the FastCGI server process *)
  if is_dir fastcgi then begin
    let _ = Sys.command (Printf.sprintf "svc -d %s" (Filename.quote fastcgi)) in
    system (Printf.sprintf "rm -rf %s" (Filename.quote fastcgi))
      "Could not remove the FastCGI directory"
  end ;

  (* Removing the socket *)
  if file_exists www_socket then 
    unlink www_socket 

let start () = 
  let bin_server = bin_server () in
  let www_socket = www_socket () in
  let fastcgi    = fastcgi () in
  stop ~apache_too:true ;
  mkdir fastcgi 0o755 ;
  let _ = 
    putfile (Filename.concat fastcgi "run") 
      (Printf.sprintf "#!/bin/sh\nspawn-fcgi -n -s %s -f %s\n" www_socket bin_server) ;
  in
  system (Printf.sprintf "chmod u+x %s" (Filename.concat fastcgi "run")) 
    "Could not install FastCGI supervisor" ;
  system (Printf.sprintf "supervise %s &" fastcgi) 
    "Could not start FastCGI process" ;

  let command = Printf.sprintf "chmod a+w %s" www_socket in
  system ~tries:3 command "Could not allow web server to write to FastCGI socket" 
  
      
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

