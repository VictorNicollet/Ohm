type 'a t = 'a -> 'a

let default_hash_size = 20

let list compress = 
  let hash = Hashtbl.create default_hash_size in
  let rec recurse = function [] -> [] | (head :: tail) as list ->
    try Hashtbl.find hash list with Not_found -> 
      let list' = compress head :: recurse tail in     
      Hashtbl.add hash list' list' ;
      list'
  in recurse

let option compress = 
  let hash = Hashtbl.create default_hash_size in 
  function None -> None | Some inner as value -> 
    try Hashtbl.find hash value with Not_found ->
      let value' = Some (compress inner) in
      Hashtbl.add hash value' value' ;
      value' 

let basic () = 
  let hash = Hashtbl.create default_hash_size in
  fun basic -> 
    try Hashtbl.find hash basic with Not_found -> 
      Hashtbl.add hash basic basic ;
      basic

let pair compress_fst compress_snd = 
  let hash = Hashtbl.create default_hash_size in
  function (fst,snd) as pair ->
    try Hashtbl.find hash pair with Not_found ->
      let pair' = compress_fst fst, compress_snd snd in
      Hashtbl.add hash pair' pair' ;
      pair'

let simple recurse = 
  let hash = Hashtbl.create default_hash_size in
  fun value -> 
    try Hashtbl.find hash value with Not_found -> 
      let value' = recurse value in 
      Hashtbl.add hash value' value' ;
      value' 

let keep x = x

let any project recurse = 
  let hash = Hashtbl.create default_hash_size in
  fun value -> 
    let key = project value in
    try Hashtbl.find hash key with Not_found -> 
      let value' = recurse value in 
      Hashtbl.add hash key value' ;
      value'
