(* Ohm is © 2012 Victor Nicollet *)

open Ohm
open Ohm.Universal
open BatPervasives

(** {1 Instance role} *)

(** The role of the current instance. This value can be used to only perform 
    some actions in a certain context, such as the web server or the asynchronous 
    bot. 
*)
let role = Util.role () 

(** {1 Environment and basic configuration} *)

(** Available environments. These represent the contexts in which the 
    application may be deployed, such as a production server or the
    local machine of a developer.
*)
type environment = [ `Dev | `Prod ]

(** The current {!type:environment}. *)
let environment = `Dev 

(** A string representing the current environment, which is then used to 
    construct various environment-dependent names and identifiers.
*)
let env = match environment with 
  | `Prod    -> "prod"
  | `Dev     -> "dev"

(** The full absolute path to the log file.
*)
let logpath = match role with 
  | `Put
  | `Reset -> None
  | `Bot
  | `Web   -> Some ("/var/log/ohm/" ^ ConfigProject.lname ^ "-" ^ env ^ ".log")

let () = 
  Configure.set `Log (BatOption.default "-" logpath)

(** {1 Database management} *)

(** This function returns the full name of a database, constructed from 
    the local name (what the database is called in this project), the 
    project name and the current environment name.
*)
let db name = Printf.sprintf "%s-%s-%s" ConfigProject.lname env name

(** The configuration database. Plug-ins expect to find their 
    configuration in this database. The key for this database is
    ["config"].
*)
module ConfigDB = CouchDB.Convenience.Database(struct let db = db "config" end)

(** The {b configuration} for the asynchronous processing database. 
    This is were asynchronous tasks are stored. The key for this 
    database is ["async"].
*)
module AsyncDB = CouchDB.Convenience.Config(struct let db = db "async" end)

(** {1 Standard execution context} *)

(** The type of internationalization keys available for translation by
    the standard context. The context provides the infrastructure 
    required to turn keys of this type into actual translated strings.
*)
type i18n = Asset_AdLib.key

(** The standard context class. Provides: {ul
    {- CouchDB interaction}
    {- Async task scheduling}
    {- AdLib internationalization}}
    Use the {!val:ctx} function below to create instances of this class.
*)
class ctx adlib = object
  inherit CouchDB.init_ctx
  inherit Async.ctx
  inherit [i18n] AdLib.ctx adlib
end

(** Create a new context for a specific language. 
*)
let ctx = function
  | `EN -> new ctx Asset_AdLib.en

(** The type of a pre-emptive thread working in a standard context. 
*)
type 'a run = (ctx,'a) Run.t

(** {1 Instances} *)

(** The reset module responsible for sending "shut down and reboot" signals
    to all running instances of the application through the database. 
*)
module Reset    = Reset.Make(ConfigDB)

(** Run an action when the instance is running in [`Put] mode. 
*)
let put action = 
  if role = `Put then 
    ignore (Ohm.Run.eval (ctx `EN) action) 

(** {1 Rendering a page} *)

(** The CSS files used by all the pages rendered using {!val:page}. This
    should usually include [Asset.css], which is the CSS file generated
    by the asset pipeline.
*)
let common_css = [
  Asset.css
]

(** The Javascript files used by all the pages rendered using {!val:page}.
    This should usually include [Asset.js], which is the CSS file generated
    by the asset pipeline, as well as jQuery.
*)
let common_js = [
  "https://ajax.googleapis.com/ajax/libs/jquery/1.8.1/jquery.min.js" ;
  Asset.js 
]

(** Rendering a page. The caller may provide additional CSS and javascript
    files to be added. 
    
    Be aware that many plugins rely on this function being defined.
*)
let page ?(css=[]) ?(js=[]) ?head ?favicon ?body_classes ~title writer =
  let css = common_css @ css in
  let js  = common_js  @ js  in
  Ohm.Html.print_page ~css ~js ?head ?favicon ?body_classes ~title writer

(** {1 Asynchronous tasks} *)

(** The Async module. Instead of using this module directly, use the
    {!val:async} object below.
*)
module Async = Ohm.Async.Make(AsyncDB)

(** Asynchronous task manager. Use this object to spawn asynchronous 
    tasks, or register periodic tasks. 
*) 
let async : ctx Async.manager = new Async.manager

(** This function is used by {!module:Main} to run tasks as part of the
    [`Bot] role of the application. 
*)
let run_async () = 
  async # run (fun () -> ctx `EN) 

(** {1 Web configuration} *)

(** The domain on which the server should respond to requests. 
*)
let domain = match environment with 
  | `Prod -> ConfigProject.lname ^ ".com"
  | `Dev  -> ConfigProject.lname ^ ".local"

(** The domain suffix on which cookies are published. 
*)
let cookies = "." ^ domain

(** The snigle-domain server configuration, used to bind request
    handlers.
*)
let server = Action.Convenience.single_domain_server ~cookies domain

(** Runs an action body within the standard context in the default
    language. 
*)
let action f req res = Run.with_context (ctx `EN) (f req res) 

(** Defines the 404 error *)
let register_404 f = Action.register_404 
  (fun server page res -> Run.with_context (ctx `EN) (f server page res))

(** Bind a new action, providing the body to be executed. This 
    returns an endpoint which can be used to generate URLs. 
*)
let register url args body = 
  Action.register server url args (action body)      

(** Bind a new action, but provide the body later. This returns 
    both an endpoint, and a function to be called on the body. 
*)
let declare url args = 
  let endpoint, define = Action.declare server url args in
  endpoint, action |- define

