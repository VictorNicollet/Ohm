open Util
open BatPervasives

module type KEY = sig
  val key : string
end

module type PROOF = sig

  val prove    : string list -> string
  val is_proof : string -> string list -> bool

  val passhash : string -> string

end

module Make = functor(Key:KEY) -> struct

  let _key days_ago =   
    let time = Util.string_of_time (Unix.time () -. 3600. *. 24. *. (float_of_int days_ago)) in
    let time = String.sub time 0 8 in
    let key  = Sha1.to_hex (Sha1.string (Key.key ^ time ^ Key.key)) in
    key

  let _key_max_age = 3
    
  let _prove key what =   
    let sha = 
      ("" :: what) @ [""]
      |> String.concat key
      |> Sha1.string
      |> Sha1.to_hex
    in
    let rec aux acc num i = 
      let (acc,num) = if i = 40 || i mod 7 = 6 then  
	  acc ^ base62 5 num, 0
	else acc, num in
      if i = 40 then acc else 
	let num = num * 16 + dec_of_hex_char sha.[i] 
	in aux acc num (i+1)	
    in 
    
    aux "" 0 0    
      
  let prove what = _prove (_key 0) what
    
  let is_proof proof what = 
    let rec aux n = 
      n < _key_max_age && (
	proof = _prove (_key n) what || aux (n+1)
      )
    in aux 0
    
  let passhash password = _prove Key.key [ "password" ; password ] 
    
end
