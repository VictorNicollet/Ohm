(* Ohm is © 2012 Victor Nicollet *)

(** The {b Controller} layer: HTTP requests, dispatching, and responses. 

    In normal use situations, the dispatch mechanism is already taken care of by module {!Main}, so 
    all you have to do is define servers and controllers. 

    Whenever the application receives a new request, {!Action.Make.dispatch} would be called
    to select the appropriate action and run it, returning the results. The easiest way to do this is to 
    use [ocamlnet2] FastCGI functionality:

    {[ at_exit (fun () -> Netcgi_fcgi.run MyAction.dispatch) ]}  

    @author Victor Nicollet
    @version 1.0
*)

(** A server type. A server represents one or more domains, ports and protocols. If more than one 
    domain is supported, then the server has a parameter which specifies which is
    being used (either for receiving a request, or for generating some HTML). 
*)
class type ['param] server = object

  (** Is this server HTTP or HTTPS ? *)
  method protocol : [`HTTP|`HTTPS]

  (** What is the domain name for this server ? *)
  method domain : 'param -> string

  (** What is the port for this server ? *)
  method port : 'param -> int

  (** What is the cookie domain name for this server ? Cookies emitted by this server will be bound
      to this domain, which may make them available to other servers based on the standard cookie
      rules. Return [None] to set the cookie only for the current domain. *)
  method cookie_domain : string option

  (** Does this server match an incoming request ? If so, extract the parameter that can be used to
      generate another request on this domain. *)
  method matches : [`HTTP|`HTTPS] -> string -> int -> 'param option

end

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
    
(** {b HTML Page}: responds with a web page. The contents are provided as a function that 
    takes a bit of javascript as argument and returns a string, for instance the function 
    returned by {!val:Html.render_page}. 
    
    {[
Action.register (...) begin fun request response -> 
  let html = Html.str "<b>Hello, world!</b>" in
  return $ Act.page (Html.render_page html) response
end
    ]}
*)
val page : (JsCode.t -> string) -> response -> response

(** {b Redirect}: creates a 303 See Other HTTP redirect to the specified absolute URL. Data and JavaScript
    channels are erased, Cookies are kept.

      {[
Action.register (...) begin fun request response -> 
  return $ Act.redirect "http://www.example.com/foo/bar?qux=baz" response
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
Action.register (...) begin fun request response ->
  let json = [
    "ok", Json_type.Build.list Json_type.Build.int [ 1 ; 2 ; 3 ]
  ] in

  return $ Act.json json response
end
    ]}
*)
val json : (string * Json_type.t) list -> response -> response

(** {b Cookies}: add a cookie on top of another response. The cookie lifetime is specified in 
    seconds starting from the time the response is generated ({i not} 01/01/1970), a value of
    zero means the cookie disappears when the browser is closed. Cookies are independent
    of Data and JavaScript channels.

    {[
Action.register (...) begin fun request response -> 
  let cookie_name  = "SESSION" in
  let cookie_value = "ses-13F438A" in
  let cookie_life  = 3600 in 
  let url = "http://www.example.com/after-login" in

  return $ Act.with_cookie ~name:cookie_name ~value:cookie_value ~life:cookie_life response
end 
    ]}
    
    It is of course possible to set multiple cookies in one response.
*)
val with_cookie : name:string -> value:string -> life:int -> response -> response

(** {b Files}: return an attached file for downloading. One should provide the
    file name, the MIME type of the file, and a view used to render the file
    data itself. This overwrites any Data and JavaScript channels, but conserves Cookies.

    {[
Act.register (...) begin fun request response ->
  let mime = "text/plain"
  let file = "hello.txt"
  let data = "Hello, world!"

  return $ Act.file ~file ~mime ~data response
end 
    ]}
*)
val file : file:string -> mime:string -> data:string -> response -> response

(** {b JavaScript}: attaches some JavaScript to be executed after an HTML or JSON response. 
    If the response is HTML, the view will receive the JavaScript code (turned to a string) as
    a parameter to insert it into an appropriate script tag. If the response is JSON, a [code]
    field will be added to the final JSON (if it's not an object, it will become one and previous
    data will be lost). JavaScript appears in the same order it was added to the response.
    
    If the Data channel is neither HTML nor JSON, it is reset with an empty JSON object. If this
    is not the desired behavior, use {!Action.Response.more_javascript} instead.
    
    {[
Action.register (...) begin fun request response ->
  let code = JsBase.init in
  return $ Act.javascript code response
end      
    ]}
*)
val javascript : JsCode.t -> response -> response

(** {b Append-Only JavaScript}: works as {!Action.Response.javascript}, but does not overwrite
    the response if it was neither HTML nor JSON.

    {[
Act.register (...) begin fun request response ->
  let code = JsBase.init in
  return $ Act.more_javascript code response
end      
    ]}
  *)
val more_javascript : JsCode.t -> response -> response

(** The data carried by an HTTP request. 
*)
class type ['server,'args] request = object
 
  (** The server-provided data, of the same type as the server parameter. 
  *)
  method server : 'server
    
  (** The path that appears after the domain name in the URL. 
      
      Note that the path is cleaned by removing initial and trailing slashes, as well as the query string. 
      So, [http://domain.com/a/b/c/?q=foo] yields a path of [a/b/c].
  *)
  method path : string
      
  (** Extract the request data which has been provided either as a field-and-value standard [`POST]
      or as a single [`JSON] value. 
  *)
  method post : [ `JSON of Json_type.t | `POST of (string,string) BatPMap.t ] option
  
  (** Extract additional [GET] parameters from the query string. 
  *)
  method get : string -> string option  
  
  (** The arguments extracted by the wildcard section of the path.
  *)
  method args : 'args

  (** Extracts the value of a cookie, if set. 
      
      If a cookie [SESSION=0Z23yB] is sent by the browser, then [request # cookie "SESSION"] will
      return [Some "0Z23yB"]. If no cookie by that name is sent, the method returns [None]. 
  *)
  method cookie : string -> string option
          
end

(** An action - provided by user code to respond to requests. 
    
    Actions are registered with the system using {!val:Action.Make.register}. Then, {!val:Action.Make.dispatch}
    finds the appropriate action to respond to a given HTTP request based on the provided {!class:Action.Make.controller}
    and calls the function to obtain the response, which is then sent back to the
    client. 
*)    
type ('args,'params) t = ('args,'params) request -> response -> (unit,response) Run.t

(** Parsing arguments. 

    It determines how segments present in the provided path after the matched path prefix. 
    That is, if an action matches path prefix ["foo/bar"] and a request provides path
    ["foo/bar/baz/quux"], then the argument parser decides what should happen with 
    segments ["baz"] and ["quux"], and whether they should be accepted or not. 
*)
module Args : sig
    
  (** The type of a cell parser - a two-way function that turns strings into the 
      ultimate type, and back. The from-string conversion may fail, which causes
      the match to fail as well. 
  *)
  type 'a cell = ('a -> string) * (string -> 'a option) 

  (** A string cell. *)
  val string : string cell

  (** An integer cell. *)
  val int : int cell

  (** The type of an argument parser. 
  *)
  type 'args t = ('args -> string list) * (string list -> 'args option)

  (** Try to parse an argument list using a parser. *)
  val parse : 'args t -> string list -> 'args option

  (** Generate a string list from a parser and some arguments. *)
  val generate : 'args t -> 'args -> string list

  (** No arguments. If the provided path is not equal to the path prefix, then no match
      occurs and the action is not executed. 
  *)
  val none : unit t

  (** The functions below are all built on the same naming principles [[ro]{0,4}[in]?]
      Each [r] indicates a required argument. Each [o] indicates an optional argument. 
      A final [n] indicates that additional parameters are returned as a list. A final
      [i] indicates that additional parameters are ignored. 
  *)
    
  val r : 'a cell -> 'a t
  val rr : 'a cell -> 'b cell -> ('a * 'b) t
  val rrr : 'a cell -> 'b cell -> 'c cell -> ('a * 'b * 'c) t
  val rrrr : 'a cell -> 'b cell -> 'c cell -> 'd cell -> ('a * 'b * 'c * 'd) t
  val o : 'a cell -> 'a option t
  val ro : 'a cell -> 'b cell -> ('a * 'b option) t
  val oo : 'a cell -> 'b cell -> ('a option * 'b option) t
  val rro : 'a cell -> 'b cell -> 'c cell -> ('a * 'b * 'c option) t
  val roo : 'a cell -> 'b cell -> 'c cell -> ('a * 'b option * 'c option) t
  val ooo : 'a cell -> 'b cell -> 'c cell -> ('a option * 'b option * 'c option) t
  val rrro : 'a cell -> 'b cell -> 'c cell -> 'd cell -> ('a * 'b * 'c * 'd option) t
  val rroo : 'a cell -> 'b cell -> 'c cell -> 'd cell -> ('a * 'b * 'c option * 'd option) t
  val rooo : 'a cell -> 'b cell -> 'c cell -> 'd cell -> ('a * 'b option * 'c option * 'd option) t
  val oooo : 'a cell -> 'b cell -> 'c cell -> 'd cell -> ('a option * 'b option * 'c option * 'd option) t

  val i : unit t
  val ri : 'a cell -> 'a t
  val rri : 'a cell -> 'b cell -> ('a * 'b) t
  val rrri : 'a cell -> 'b cell -> 'c cell -> ('a * 'b * 'c) t
  val rrrri : 'a cell -> 'b cell -> 'c cell -> 'd cell -> ('a * 'b * 'c * 'd) t
  val oi : 'a cell -> 'a option t
  val roi : 'a cell -> 'b cell -> ('a * 'b option) t
  val ooi : 'a cell -> 'b cell -> ('a option * 'b option) t
  val rroi : 'a cell -> 'b cell -> 'c cell -> ('a * 'b * 'c option) t
  val rooi : 'a cell -> 'b cell -> 'c cell -> ('a * 'b option * 'c option) t
  val oooi : 'a cell -> 'b cell -> 'c cell -> ('a option * 'b option * 'c option) t
  val rrroi : 'a cell -> 'b cell -> 'c cell -> 'd cell -> ('a * 'b * 'c * 'd option) t
  val rrooi : 'a cell -> 'b cell -> 'c cell -> 'd cell -> ('a * 'b * 'c option * 'd option) t
  val roooi : 'a cell -> 'b cell -> 'c cell -> 'd cell -> ('a * 'b option * 'c option * 'd option) t
  val ooooi : 'a cell -> 'b cell -> 'c cell -> 'd cell -> ('a option * 'b option * 'c option * 'd option) t

  val n : 'a cell -> 'a list t
  val rn : 'a cell -> 'l cell -> ('a * 'l list) t
  val rrn : 'a cell -> 'b cell -> 'l cell -> ('a * 'b * 'l list) t
  val rrrn : 'a cell -> 'b cell -> 'c cell -> 'l cell -> ('a * 'b * 'c * 'l list) t
  val rrrrn : 'a cell -> 'b cell -> 'c cell -> 'd cell -> 'l cell -> ('a * 'b * 'c * 'd * 'l list) t
  val on : 'a cell -> 'l cell -> ('a option * 'l list) t
  val ron : 'a cell -> 'b cell -> 'l cell -> ('a * 'b option * 'l list) t
  val oon : 'a cell -> 'b cell -> 'l cell -> ('a option * 'b option * 'l list) t
  val rron : 'a cell -> 'b cell -> 'c cell -> 'l cell -> ('a * 'b * 'c option * 'l list) t
  val roon : 'a cell -> 'b cell -> 'c cell -> 'l cell -> ('a * 'b option * 'c option * 'l list) t
  val ooon : 'a cell -> 'b cell -> 'c cell -> 'l cell -> ('a option * 'b option * 'c option * 'l list) t
  val rrron : 'a cell -> 'b cell -> 'c cell -> 'd cell -> 'l cell -> ('a * 'b * 'c * 'd option * 'l list) t
  val rroon : 'a cell -> 'b cell -> 'c cell -> 'd cell -> 'l cell -> ('a * 'b * 'c option * 'd option * 'l list) t
  val rooon : 'a cell -> 'b cell -> 'c cell -> 'd cell -> 'l cell -> ('a * 'b option * 'c option * 'd option * 'l list) t
  val oooon : 'a cell -> 'b cell -> 'c cell -> 'd cell -> 'l cell -> ('a option * 'b option * 'c option * 'd option * 'l list) t

end

(** Raised when no actions match a specific request. 
*)
exception Action_not_found of string
      
(** An endpoint is a controller that has been bound to an action and can be converted to an URL
    by receiving all the parameters required to fill in the path and domain. *)
type ('server,'args) endpoint

(** The URL of an endpoint. *)
val url : ('server,'args) endpoint -> 'server -> 'args -> string
      
(** Dispatch a FastCGI request. 
    
    This function extracts the appropriate action, runs it, then sends the result back 
    to the HTTP client.
    
    This should be handled by module {!Main}. If not, you can use: 
    
    {[ at_exit (fun () -> Action.run Action.dispatch) ]}  
    
    Here, [at_exit] is used to ensure that all global calls to {!val:Action.register} have been 
    performed before [Netcgi_fcgi.run] is called. 
    
    @raise Action.Make.Action_not_found if no matching action is found. 
*)
val dispatch : #Netcgi.cgi -> unit

(** Register an action with the dispatcher.
    
    This allows the action to be found by {!val:Action.resolve} and {!val:Action.dispatch}.

    Registering an action returns an endpoint that you can use to 
*)
val register : 
     'server server
  -> string
  -> 'args Args.t
  -> ('server,'args) t 
  -> ('server,'args) endpoint

(** Declare an action with the dispatcher. This helps return the endpoint before the actual
    action has been defined, and so helps with mutually recursive functions or simply
    defining [Url] modules that contain undefined endpoints. 

    {[
(* urls.ml *)
let endpoint, define = Action.declare controller 

(* actions.ml *)
let () = Urls.define action
    ]}

    A warning will be logged if {!Action.dispatch}, {!Action.resolve} or {!Action.run}
    are called before all declared endpoints are defined, or if an endpoint is defined 
    twice.

*)
val declare : 
     'server server
  -> string
  -> 'args Args.t
  -> ('server,'args) endpoint * (('server,'args) t -> unit) 

(** Run the Fastcgi server with the appropriate default configuration. *)
val run : (Netcgi_fcgi.cgi -> unit) -> unit
  
(** Helper functions for your convenience. *)
module Convenience : sig

  (** A server that responds to a single domain.
      [let server = single_domain_server "www.domain.com"].
  *)
  val single_domain_server : ?secure:bool -> ?port:int -> ?cookies:string -> string -> unit server

  (** A server that responds to multiple subdomains of a given domain.
      [let server = sub_comain_server ".domain.com"] would match [foo.domain.com] but 
      not [domain.com].
  *)
  val sub_domain_server : ?secure:bool -> ?port:int -> ?cookies:string -> string -> string server

  (** Generate the root URL of a server. This returns a string of the 
      form [http://example.com:666] 
  *)
  val root : 'param server -> 'param -> string

  (** A request that carries no cookies, get or post data, it only contains server and path info
      provided upon creation. This function is not typically useful, but serves as a convenient
      when using {!Action.resolve}.
  *)
  val nilreq : 'server -> 'args -> ('server,'args) request

end
