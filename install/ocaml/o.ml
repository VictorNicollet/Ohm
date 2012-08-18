(* Ohm is © 2012 Victor Nicollet *)

open Ohm
open Ohm.Universal
open BatPervasives

(* Environment and basic configuration ---------------------------------------------------------------------- *)

let environment = `Dev 

let env = match environment with 
  | `Prod    -> "prod"
  | `Dev     -> "dev"

let () = 
  Configure.set `Log begin match Ohm.Util.role with 
    | `Put
    | `Reset -> "-"
    | `Bot
    | `Web   -> "/var/log/ohm/" ^ env ^ ".log"
  end

(* Basic databases ------------------------------------------------------------------------------------------ *)

let db name = Printf.sprintf "%s-%s" env name

module ConfigDB = CouchDB.Convenience.Database(struct let db = db "config" end)
module Reset    = Reset.Make(ConfigDB)

(* Context management --------------------------------------------------------------------------------------- *)

type i18n = Asset_AdLib.key

class ctx adlib = object
  inherit CouchDB.init_ctx
  inherit Async.ctx
  inherit [i18n] AdLib.ctx adlib
end

let ctx = function
  | `FR -> new ctx Asset_AdLib.fr

let put action = 
  if Ohm.Util.role = `Put then 
    ignore (Ohm.Run.eval (ctx `FR) action) 

type 'a run = (ctx,'a) Run.t

module AsyncDB = CouchDB.Convenience.Config(struct let db = db "async" end)
module Async = Ohm.Async.Make(AsyncDB)

let async : ctx Async.manager = new Async.manager

let run_async () = 
  async # run (fun () -> ctx `FR) 

(* Action management ---------------------------------------------------------------------------------------- *)

let domain = match environment with 
  | `Prod -> "project.com"
  | `Dev  -> "project.local"

let cookies = "." ^ domain

let core   = Action.Convenience.single_domain_server ~cookies domain

let action f req res = 
  Run.with_context (ctx `FR) (f req res)

let register s u a body = 
  Action.register s u a (action body)

let declare s u a = 
  let endpoint, define = Action.declare s u a in
  endpoint, action |- define

