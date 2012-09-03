(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives

open Common 

let start () = 
  if is_dir (Filename.concat Path.bot "supervise") then
    system (Printf.sprintf "svc -u %s" (Filename.quote Path.bot)) 
      "Could not start asynchronous bot process"      
  else
    system (Printf.sprintf "supervise %s &" (Filename.quote Path.bot)) 
      "Could not start asynchronous bot process" 

let stop () = 
  system (Printf.sprintf "svc -d %s" (Filename.quote Path.bot)) 
    "Could not stop asynchronous bot process"

let tool = function 
  | []
  | "start" :: _ -> start ()
  | "stop"  :: "start" :: _ -> stop () ; start ()
  | "stop"  :: _ -> stop () 
  | _ -> error "Unrecognized bot-tool instruction"
    "Expected 'ohm bot start' or 'ohm bot stop'."
