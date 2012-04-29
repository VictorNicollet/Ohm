(* Ohm is © 2011 Victor Nicollet *)

type role = [ `Bot | `Web | `Put | `Reset ]

let pid   = Unix.getpid ()

let role =
  let bot     = ref false 
  and put     = ref false 
  and cgi     = ref false 
  and reset   = ref false in 
  Arg.parse [
    "--reset", Arg.Set reset, "force other processes to restart" ;
    "--cgi",   Arg.Set cgi,   "run as FastCGI (default)" ;
    "--put",   Arg.Set put,   "run as view/i18n uploader" ;
    "--bot",   Arg.Set bot,   "run as bot" ;
  ] (fun _ -> ()) "Start an instance of the Ohm server" ;
  if !bot then `Bot else 
    if !put then `Put else
	if !reset then `Reset else `Web

module Logging = struct

  let open_channel = 
    let chanref = ref None in 
    fun () -> 
      match !chanref with 
	| None -> 
	  let path = Configure.lock `Log in 
	  let chan = 
	    if path = "-" then stdout
	    else open_out_gen [Open_append] 0666 path
	  in
	  chanref := Some chan ; chan
	| Some chan -> chan 

  let prefix  = 
    Printf.sprintf "[%s:%d]" 
      (match role with 
	| `Reset -> "RESET"
	| `Bot   -> "BOT"
	| `Web   -> "WEB"
	| `Put   -> "PUT")
      pid 

  let output string = 
    let channel = open_channel () in 
    let time = Unix.localtime (Unix.gettimeofday ()) in
    let string =
      Printf.sprintf "[%d/%02d/%02d %02d:%02d:%02d] %s %s\n" 
	(time.Unix.tm_year + 1900)
	(1 + time.Unix.tm_mon)
	(time.Unix.tm_mday)
	(time.Unix.tm_hour)
	(time.Unix.tm_min)
	(time.Unix.tm_sec)
        prefix 
	string 
    in
    output_string channel string ;
    flush channel 

end

let string_of_time time = 
  let time = Unix.gmtime time in 
  Printf.sprintf "%04d%02d%02d%02d%02d%02d"
    (time.Unix.tm_year + 1900)
    (1 + time.Unix.tm_mon)
    (time.Unix.tm_mday)
    (time.Unix.tm_hour)
    (time.Unix.tm_min)
    (time.Unix.tm_sec)
    
let log format = 
  Printf.ksprintf Logging.output format

let memoize f = 
  let h = Hashtbl.create 10 in
  fun x -> 
    try Hashtbl.find h x with Not_found -> 
      let y = f x in Hashtbl.add h x y ; y


let get_binary_contents full = 
  try 
    let chan = open_in_bin full in 
    let size = in_channel_length chan in 
    let into = String.create size in
    let rec readall i size = 
      let read = input chan into i size in
      if read = size then () else 
	readall (i + read) (size - read) 
    in readall 0 size ; 
    close_in chan ;
    Some into 
  with
    | exn ->
      log "Util.get_contents: could not open %s (%s)" full (Printexc.to_string exn);
      None


let urlencode str = 
  let regexp = Str.regexp "[^-a-zA-Z0-9$_.+!*'(),]" in
  let result = Str.global_substitute regexp begin fun str ->
    let c = Str.matched_string str in 
    Printf.sprintf "%%%0x" (Char.code c.[0])
  end str in
  result

let get_contents file = 
  try 
    let chan = open_in file in 
    let size = in_channel_length chan in 
    let into = String.create size in
    let rec readall i size = 
      let read = input chan into i size in
      if read = size then () else 
	readall (i + read) (size - read) 
    in readall 0 size ; 
    close_in chan ;
    BatUTF8.validate into ;
    Some into 
  with
    | BatUTF8.Malformed_code ->
      log "Util.get_contents: file %s is not valid utf-8" file ;
      None
    | exn ->
      log "Util.get_contents: could not open %s (%s)" file (Printexc.to_string exn) ;
      None

let get_view_contents file = 
  let view_dir = Configure.lock `Templates in
  get_contents (view_dir ^ file)

let get_resource_contents file = 
  let resource_dir = Configure.lock `Resources in
  get_contents (resource_dir ^ file)

let log_requests = false

let logreq = 
  if log_requests then log
  else function format -> Printf.ksprintf ignore format
	
let logjson js = 
  Json_io.string_of_json ~recursive:true js
  
let _uniq_b = ref 0
let _uniq_c = Unix.getpid ()

let seq_old = "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ0123456789"
let seq_cdb = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

let base62 seq n i = 
  let r = String.make n seq.[0] in
  let rec aux i k = 
    if i <> 0 then begin
      r.[k] <- seq.[i mod 62] ;
      aux (i / 62) (k - 1)
    end
  in aux i (n-1) ; r

let uniq () = 
  let a = int_of_float (Unix.time() -. 1286058501.0)
  and b = incr _uniq_b ; !_uniq_b mod 238328
  and c = _uniq_c
  in (base62 seq_cdb 5 a)^(base62 seq_cdb 3 b)^(base62 seq_cdb 3 c)

let base62 = base62 seq_old

let base62_of_int = 
  base62 6

let base62_to_base34 =

  let ascii_zero = Char.code '0' in
  let ascii_a    = Char.code 'a' in
  let ascii_A    = Char.code 'A' in

  let to_int = function
    | ('0' .. '9') as c -> Char.code c - ascii_zero
    | ('a' .. 'z') as c -> Char.code c - ascii_a + 10
    | ('A' .. 'Z') as c -> Char.code c - ascii_A + 10 + 26
    | _ -> 0
  in

  let rec base62_extract s i l =
    try if l = 0 then 0 else to_int s.[i] + 62 * base62_extract s (i+1) (l-1) with _ -> 0
  in

  let base34 = "0123456789ABCDEFGHJKLMNPQRSTUVWXYZ" in

  let rec base34_write s i l value =                    
    if l = 0 then () else (                             
      s.[i] <- base34.[value mod 34] ;                  
      base34_write s (i+1) (l-1) (value / 34)           
    )                                              
  in

  fun input ->
    let s = "xxxxx-xxxxx-xxxx" in
    base34_write s  0 5 (base62_extract input 0 4) ;
    base34_write s  6 5 (base62_extract input 4 4) ;
    base34_write s 12 4 (base62_extract input 8 3) ;
    s                                                            

let dec_of_hex_char = function
  | '0' -> 0  | '1' -> 1  | '2' -> 2  | '3' -> 3
  | '4' -> 4  | '5' -> 5  | '6' -> 6  | '7' -> 7
  | '8' -> 8  | '9' -> 9  | 'a' -> 10 | 'b' -> 11
  | 'c' -> 12 | 'd' -> 13 | 'e' -> 14 | 'f' -> 15
  |  _  -> 0

let sha1 string = 
  let hex = Sha1.to_hex (Sha1.string string) in
  let len = String.length hex in
  
  BatString.init     
    (len/2)  
    (fun i -> Char.chr (dec_of_hex_char hex.[2*i] * 16 + dec_of_hex_char hex.[2*i+1]))

let sha1_hmac (o_key_pad,i_key_pad) text = 
  BatBase64.str_encode (sha1 (o_key_pad ^ sha1 (i_key_pad ^ text))) 

let utf8 string = 
  try BatUTF8.validate string ; Some string 
  with BatUTF8.Malformed_code -> None

let rec last = function
  | []   -> None
  | [x]  -> Some x
  | _::t -> last t

let first = function
  | []   -> None
  | h::_ -> Some h

let rec setdiff cmp a b = match a,b with 
  | _, [] -> a
  | [], _ -> []
  | ha :: ta, hb :: tb -> 
    let c = cmp ha hb in 
    if c = 0 then setdiff cmp ta tb
    else if c < 0 then ha :: setdiff cmp ta tb
    else setdiff cmp a tb

let rec setand cmp a b = match a,b with 
  | _, [] -> [] 
  | [], _ -> []
  | ha :: ta, hb :: tb ->
    let c = cmp ha hb in 
    if c = 0 then ha :: (setand cmp ta tb) 
    else if c < 0 then setand cmp ta b else setand cmp a tb

let fold_accents text = 
  List.fold_left (fun text (reg,rep) -> Str.global_replace (Str.regexp reg) rep text) text
    [ "à\\|À\\|â\\|Â\\|ä\\|Ä"         , "a" ;
      "é\\|É\\|ê\\|Ê\\|è\\|È\\|ë\\|Ë" , "e" ; 
      "ç\\|Ç"                         , "c" ;
      "î\\|Î\\|ï\\|Ï"                 , "i" ;
      "ù\\|Ù\\|û\\|Û\\|ü\\|Ü"         , "u" ;
      "ô\\|Ô\\|ö\\|Ö"                 , "o" ;
      "œ\\|Œ"                         , "oe" ;
    ]

let uppercase s =
  let s = String.copy s in
  for i = 0 to String.length s - 1 do
    let c = Char.code s.[i] in
    if c >= 97 && c <= 122 then
      s.[i] <- Char.chr (c - 32)
  done ; s

let remove_bom text = 
  if BatString.starts_with text "\239\187\191" then String.sub text 3 (String.length text - 3)
  else text

let fold_all text = 
  BatString.trim (uppercase (fold_accents (remove_bom text)))

let number list = 
  let rec aux acc = function
    | [] -> []
    | h :: t -> (acc , h) :: (aux (acc+1) t)
  in aux 0 list

let clip size string = 
  if String.length string > size then String.sub string 0 size else string
  
let rec next_string string = 
  let n = String.length string in
  if n = 0 then String.make 1 (Char.chr 0) else
    let code = Char.code string.[n-1] in
    if code = 255 then next_string (String.sub string 0 (n-1)) else
      let copy = String.copy string in 
      copy.[n-1] <- Char.chr (code + 1) ;
      copy
