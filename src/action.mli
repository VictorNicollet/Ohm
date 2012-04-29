(* Ohm is © 2011 Victor Nicollet *)

(** The {b Controller} layer: HTTP requests, dispatching, and responses. 

    Whenever the application receives a new request, {!Action.Make.dispatch} would be called
    to select the appropriate action and run it, returning the results. The easiest way to do this is to 
    use [ocamlnet2] FastCGI functionality:

    {[ at_exit (fun () -> Netcgi_fcgi.run MyAction.dispatch) ]}  

    {2 Usage}

    The module should be instantiated early in the controller layer. Then, throughout that layer, 
    every module can register one or more actions by creating action functions that implement {!Action.Make.t} 
    and then registering them with {!Action.Make.register}. 

    {[
MyAction.register (...) begin fun request response -> 
  MyAction.html (fun js ctx -> ctx
    >> View.str "<DOCTYPE !html><html>...<script>" 
    >> js 
    >> View.str "</script></html>"  
  ) response
end
    ]}

    @author Victor Nicollet
    @version 1.0
*)

(** Configuration module type. Used as a parameter for {!Action.Make}.

    This is used on multi-server configurations to differentiate between requests sent
    to the various available servers : when registering an action, you specify what server
    that action corresponds to. 

    For instance, if you have two servers that respond on [blue.domain.com] and [red.domain.com],
    then you would define [type server = Blue | Red], and have [server_of_name] return
    [Blue] for [blue.domain.com] and [Red] for [red.domain.com].

    If you don't need to handle multiple domain names, you can use {!Action.SingleServer}.
*)
module type CONFIG = sig 

  (** The type of a server. See {!Action.CONFIG} for more information on servers. 
      
      A typical implementation in a single-server setup could be {[ type server = TheServer ]}
  *)
  type server 

  (** The domain name of a server. See {!Action.CONFIG} for more information on servers.

      This is used for logging and debugging purposes. It would make sense that:

      {[ server_of_name (name_of_server server) = server ]}

      However, this is not necessary (except for your own sanity while debugging). 

      In a single-server setup for the domain name [www.example.com], a typical implementation
      could be {[ let name_of_server _ = "www.example.com" ]}
  *)
  val name_of_server : server -> string

  (** Determine the server based on the domain name. See {!Action.CONFIG} for more information
      on servers.

      This is used by the dispatcher to determine what server a given request should be
      mapped to. This function has no possibility of returning no value, and should {b not} raise
      an exception. Instead, it is advised to have a catch-all server that can serve a 
      decent 404 page. 

      A typical implementation for a single-server setup could be 
      {[ let server_of_name _ = TheServer ]} 
  *)
  val server_of_name : string -> server

  (** The domain suffix for multi-domain cookies.
      
      When a cookie is generated, it may be shared across several domains that have the same 
      suffix. That suffix is returned by this function based on the current server.
      So, if a given server matches all domains of the form [*.domain.com], using a suffix
      of [.domain.com] would make the cookie available to all those domains.

      If no suffix is returned, the cookie will only be available to the precise domain that 
      generated it, so [red.domain.com] would not see a cookie set by [blue.domain.com]

      In a single-server setup, a typical implementation would be
      {[ let server_suffix _ = None ]}
  *)
  val server_suffix : server -> string option
           
end

(** An implementation of {!Action.CONFIG} when you only need to handle one domain name. *)
module SingleServer : sig

  (** A singleton type representing only one server. *)
  type server = [ `TheServer ]

  (** Returns ["..."] for all inputs. *)
  val name_of_server : server -> string

  (** Returns [`TheServer] for all inputs. *)
  val server_of_name : string -> server

  (** Returns [None] for all inputs. *)
  val server_suffix : server -> string option 

end

module type CUSTOMIZABLE = sig

(** The HTTP response. 
    
    The responsibility of the action ({!Action.Make.t}) is to transform a response into another - initially,
    an empty response is provided. Relying on transforms instead of constructing the response makes it easier
    to let several actions work on the same response, and the basic operations were designed for this. 
    
    Several transform operations are available, and they work on three distinct channels:
    
    {ul {- Data: can be some HTML, some JSON or raw data being sent as a file attachment.}
    {- Cookies: independent of any data being sent.}
    {- JavaScript: code to be executed, available when the data is either HTML or JSON}}             
    
    The initial response has a JSON Data channel containing an empty object, no cookies, and no javascript.
*)
  type response 


  (** {b Html}: create a view that renders the appropriate HTML and use the [html] constructor. This overwrites
      the contents of the Data channel (JavaScript and Cookies channels are kept). All the JavaScript code that 
      was added to the request is passed as the first argument to the view. 

      {[
module Act = Action.Make(SingleServer) 

Act.register (...) begin fun request response -> 
  Act.html (fun js ctx -> ctx
    >> View.str "<DOCTYPE !html><html>...<script>" 
    >> js 
    >> View.str "</script></html>"  
  ) response
end
      ]}
  *)
  val html : (View.Context.text View.t -> View.Context.text View.t) -> response -> response

  (** {b Redirect}: creates a 303 See Other HTTP redirect to the specified absolute URL. Data and JavaScript
      channels are erased, Cookies are kept.

      {[
module Act = Action.Make(SingleServer)

Act.register (...) begin fun request response -> 
  Act.redirect "http://www.example.com/foo/bar?qux=baz" response
end
      ]}
  *)
  val redirect : string -> response -> response

  (** {b JSON}: return JSON-formatted data. It always returns an object, as
      other values can cause security issues. So [\{ok:\[1,2,3\]\}] can be returned but [\[1,2,3\]] cannot.
      This is merged with the previously available data in the Data channel if it was JSON - merging is
      not recursive and happens on a per-field basis, so subsequent transforms adding distinct fields 
      accumulate data rather than replacing it. Be careful: the [code] field is overwritten by 
      JavaScript data.

      {[
module Act = Action.Make(SingleServer)

Act.register (...) begin fun request response ->
  let json = [
    "ok", Json_type.Build.list Json_type.Build.int [ 1 ; 2 ; 3 ]
  ] in

  Act.json json response
end
      ]}
  *)
  val json : (string * Json_type.t) list -> response -> response

  (** {b Cookies}: add a cookie on top of another response. The cookie lifetime is specified in 
      seconds starting from the time the response is generated ({i not} 01/01/1970), a value of
      zero means the cookie disappears when the browser is closed. Cookies are independent
      of Data and JavaScript channels.

      {[
module Act = Action.Make(SingleServer)

Act.register (...) begin fun request response -> 
  let cookie_name  = "SESSION" in
  let cookie_value = "ses-13F438A" in
  let cookie_life  = 3600 in 
  let url = "http://www.example.com/after-login" in

  Act.with_cookie ~name:cookie_name ~value:cookie_value ~life:cookie_life response
end 
      ]}

      It is of course possible to set multiple cookies in one response.
  *)
  val with_cookie : name:string -> value:string -> life:int -> response -> response

  (** {b Files}: return an attached file for downloading. One should provide the
      file name, the MIME type of the file, and a view used to render the file
      data itself. This overwrites any Data and JavaScript channels, but conserves Cookies.

      {[
module Act = Action.Make(SingleServer)

Act.register (...) begin fun request response ->
  let mime = "text/plain"
  let file = "hello.txt"
  let data = "Hello, world!"

  Act.file ~file ~mime ~data response
end 
      ]}
  *)
  val file : file:string -> mime:string -> data:View.Context.text View.t -> response -> response

  (** {b JavaScript}: attaches some JavaScript to be executed after an HTML or JSON response. 
      If the response is HTML, the view will receive the JavaScript code (turned to a string) as
      a parameter to insert it into an appropriate script tag. If the response is JSON, a [code]
      field will be added to the final JSON (if it's not an object, it will become one and previous
      data will be lost). JavaScript appears in the same order it was added to the response.

      If the Data channel is neither HTML nor JSON, it is reset with an empty JSON object. If this
      is not the desired behavior, use {!Action.Response.more_javascript} instead.

      {[
module Act = Action.Make(SingleServer)

Act.register (...) begin fun request response ->
  let code = JsBase.init in
  Act.javascript code response
end      
      ]}
  *)
  val javascript : JsCode.t -> response -> response

  (** {b Append-Only JavaScript}: works as {!Action.Response.javascript}, but does not overwrite
      the response if it was neither HTML nor JSON.

      {[
module Act = Action.Make(SingleServer)

Act.register (...) begin fun request response ->
  let code = JsBase.init in
  Act.more_javascript code response
end      
      ]}
  *)
  val more_javascript : JsCode.t -> response -> response

  (** The data carried by an HTTP request. 
      An object of this type is provided to the action that the request was dispatched to
      (method {!method:Action.Make.t}).
  *)
  class type request =
  object
 
    (** The path that appears after the domain name in the URL. 
	
	Note that the path is cleaned by removing initial and trailing slashes, as well as the query string. 
	So, [http://domain.com/a/b/c/?q=foo] yields a path of [a/b/c].
    *)
    method path : string

    (** The server hostname. 
	
	This is usually the domain name, possibly overwritten by the server configuration. 
	In general, [http://sub.domain.com/foo/bar] yields a server name of [sub.domain.com].
    *)
    method servername : string
      
    (** Extract the request format.

	The format determines whether request data is available through {!post} or {!json} 
	methods.
    *)
    method format : [ `JSON | `URLENCODED ]

    (** Extracts a named parameter from the request, if present. 
	
	Returns nothing if the format is not [`URLENCODED].

	If the script was called with a query string of [?foo=bar], then [request # post "foo"] yields 
	[Some "bar"] and [request # post "qux"] yields [None]. In the case of a POST request, the
	POST parameters will shadow the query string parameters with the same name. 
	
	It is not advised to use several parameters with the same name. Should this happen, only the
	last parameter will be kept.
    *)
    method post : string -> string option
      
    (** Extracts all named parameters from the request.
	
	Returns nothing if the format is not [`URLENCODED].

	The list contains key-value pairs. For instance, if the query string is [?foo=bar&baz=qux], then
	[request # postlist] yields [\[ "foo","bar" ; "baz","qux" \]]. The same applies to POST 
	parameters, if any.
    *)
    method postlist : (string * string) list

    (** Extracts the JSON content of the request.
	
	Returns [Json_type.Null] if the format is not [`JSON].
    *)
    method json : Json_type.t 
      
    (** Extracts an argument from the wildcard section of the path. 
	
	If the action path ends in a wildcard (such as [/user/*]), the path segments matched by the
	wildcard are parsed and split into a list. So, in the case of [/user/00fC3Q023jZ/edit] handled
	by an action with a path of [/user/*], [request # args 0] yields [Some "00fC3Q023jZ"], 
	[request # args 1] yields [Some "edit"] and [request # args 0] yields [None].
	
	More information about action paths can be found in {!Action.Make.controller.path}.
    *)
    method args : int -> string option

    (** Extracts the value of a cookie, if set. 
	
	If a cookie [SESSION=0Z23yB] is sent by the browser, then [request # cookie "SESSION"] will
	return [Some "0Z23yB"]. If no cookie by that name is sent, the method returns [None]. 
    *)
    method cookie : string -> string option
      
    (** The complete query received by the server. 
	
	For example, [http://www.example.com/foo/bar?qux=baz]. This method serves no practical purpose,
	except in some query logging situations. 
    *)
    method query : string 
      
    (** The IP address of the HTTP client. 
	
	The address is provided in an unspecified format (IPv4, IPv6 or a host name) and should be processed
	with appropriate IP-friendly functions of your choice before being used. It can serve as a 
	cheap-and-unstable unique visitor token if you really need one, but you really shouldn't as many ISPs
	and proxies will break such attempts. 
    *)
    method ip : string

  end

  (** An action - provided by user code to respond to requests. 

      Actions are registered with the system using {!val:Action.Make.register}. Then, {!val:Action.Make.dispatch}
      finds the appropriate action to respond to a given HTTP request based on the provided {!class:Action.Make.controller}
      and calls the function to obtain the response, which is then sent back to the
      client. 

      As such, assuming [Act = Action.Make(...)], typical actions are defined as: 

      {[
Act.register (...) begin fun request response ->
  Act.html (fun js ctx -> ctx
    >> View.str "<!DOCTYPE html><html>...<script>" 
    >> js 
    >> View.str "</script></html>"
  ) response
end
      ]}
  *)    
  type t = request -> response -> response

  (** An alias to a server type. *)
  type server 

  (** Determines what URL and server an action maps to. 

      The parameters are the server (see {!modtype:Action.CONFIG} for details) and the URL. 

      For instance, for an action to match the URL [http://red.example.com/test/url], its
      controller would be defined as: 

      {[
let _ = Act.register (new Act.controller Act.Red "test/url") action
      ]}

      The path may contain a {b wildcard} to match several different URLs with the same 
      suffix. To match the URL [http://red.example.com/view/<id>] for every possible [<id>], 
      one would define the action as: 

      {[
let _ = Act.register (new Act.controller Act.Red "view/*") action 
      ]}

      The [<id>] would then be available in the [run] method as [request # args 0] (see 
      {!method:Action.Make.request.args} for more information). 

      Note that only one wildcard is allowed, and it must be the final segment. [/foo/*/bar], 
      [/foo/bar/*/*] and [/foo/bar*] are {b illegal} and the misplaced [*] is treated as a normal character. 

      Note that without a wildcard, the action only matches the exact path that was 
      provided, so that a path of [foo/bar] would match URLs [/foo/bar/], [/foo//bar/]
      and [/foo/bar], but not [/foo/bar/qux]. 

      If several wildcard controllers match the same URL, the one with the longest initial
      match is kept (so, for [/foo/bar/qux], [/foo/bar/*] would be picked over [/foo/*]).

      It is strongly advised to provide a wildcard path ([*]) for every server to act
      as a 404 page. Otherwise, {!exception:Action.Make.Action_not_found} will be raised. 
      
  *)
  class controller : server -> string ->
  object

    (** The server on which this request is handled. See {!Action.Make.controller} for more information. *)
    method server : server

    (** The path or paths handled by this action. See {!Action.Make.controller} for more information. *)
    method path   : string

  end

  (** Raised when no actions match a specific request. Should be avoided by registering an action with 
      a {!class:Action.Make.controller} that has a path of ["*"] to handle 404 errors.
  *)
  exception Action_not_found of string
    
  (** Computes the action that matches a path. 

      This is what {!val:Action.Make.dispatch} does internally. The provided arguments are the current
      server and the complete URL path, and the function returns the action's [run] method and the
      list of segments matched by the wildcard in the action's path. 

      For instance:

      {[
module Act = Action.Make(Action.SingleServer)

let () = Act.register (new Act.controller Act.TheServer "foo/*") foo_handler

let a = Act.action_of_path Act.TheServer "foo/bar/qux"
let b = Act.action_of_path Act.TheServer "bar"
      ]}

      Here, [a] is [Some (foo_handler, \["bar";"qux"\])] and [b] is [None]. 

      Note that only actions registered with {!val:Action.Make.register} on the same module can be found 
      with this function.
      So, the following returns [None]: 

      {[
module Act = Action.Make(Action.SingleServer)

let _ = Act.register (new Act.controller Act.TheServer "foo/*") foo_handler

module Act = Action.Make(Action.SingleServer)

let a = Act.action_of_path Act.TheServer "foo/bar/qux"
      ]}

      {b Make sure your entire application only contains one call to the {!module:Action.Make} functor!}
  *)
  val action_of_path : server -> string -> ((request -> response -> response) * string list) option

  (** Dispatch a FastCGI request. 

      This function extracts the appropriate action using {!val:Action.Make.action_of_path}, calls its
      {!method:Action.Make.t.run} method and then sends the result back to the HTTP client.

      Typical usage: 

      {[ at_exit (fun () -> Act.run Act.dispatch) ]}  

      Here, [at_exit] is used to ensure that all global calls to {!val:Action.Make.register} have been 
      performed before [Netcgi_fcgi.run] is called. 

      @raise Action.Make.Action_not_found if no matching action is found. 
  *)
  val dispatch : #Netcgi.cgi -> unit

  (** Register an action with the dispatcher.

      This allows the action to be found by {!val:Action.Make.action_of_path} and {!val:Action.Make.dispatch}.
      For instance:

      {[
module Act = Action.Make(Action.SingleServer)

let () = Act.register (new Act.controller Act.TheServer "foo/*") foo_handler

let a = Act.action_of_path Act.TheServer "foo/bar/qux"
      ]}

      This sucessfully returns [Some (foo_handler, \["bar";"qux"\])].

      Note that only actions registered on the same module can be found. So, the following returns [None]: 

      {[
module Act = Action.Make(Action.SingleServer)

class action = object
  method controller = new Act.controller Act.TheServer "foo/*"
  method run = foo_handler
end

let () = Act.register (new action)

module Act = Action.Make(Action.SingleServer)

let a = Act.action_of_path Act.TheServer "foo/bar/qux"
      ]}

      {b Make sure your entire application only contains one call to the {!module:Action.Make} functor!}

      This function returns the action, just in case you need to do something with it, but most of 
      the time it will be ignored. 
  *)
  val register : #controller -> t -> unit

  (** Run the Fastcgi server with the appropriate default configuration. *)
  val run : (Netcgi_fcgi.cgi -> unit) -> unit
    
end

(** Construction functor. *)
module Customize : functor (Config:CONFIG) -> CUSTOMIZABLE with type server = Config.server

