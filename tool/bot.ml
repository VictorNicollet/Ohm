(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives

open Common 

let stop () = 

  if is_dir Path.bot then begin
    let _ = Sys.command (Printf.sprintf "svc -d %s" Path.bot) in
    system (Printf.sprintf "rm -rf %s" Path.bot)
      "Could not remove the async bot directory"
  end 

let start () = 

  stop () ;
  mkdir Path.bot 0o755 ;
  let _ = putfile (Filename.concat Path.bot "run") "#!/bin/sh\ncd ..\n./server --bot\n" in
  system (Printf.sprintf "chmod u+x %s" (Filename.concat Path.bot "run")) 
    "Could not install async bot supervisor" ;
  system (Printf.sprintf "supervise %s &" Path.bot) 
    "Could not start async bot process" 
      
let tool = function 
  | []
  | "start" :: _ -> start ()
  | "stop"  :: "start" :: _ -> stop () ; start ()
  | "stop"  :: _ -> stop () 
  | _ -> error "Unrecognized bot-tool instruction"
    "Expected 'ohm bot start' or 'ohm bot stop'."
