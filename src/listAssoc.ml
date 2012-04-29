(* Ohm is © 2011 Victor Nicollet *)

type ('key,'value) t = ('key * 'value) list

let get l k = List.assoc k l 

let try_get l k = try Some (get l k) with Not_found -> None

let set k v l = (k,v) :: l

let rec replace k v = function
  | [] -> [k,v]
  | (k',_) :: t when k' = k -> (k,v) :: t
  | h :: t -> h :: replace k v t

let rec unset k = function
  | [] -> []
  | (k',_) :: t when k' = k -> t
  | h :: t -> h :: unset k t

let rec pop k = function
  | [] -> [], raise Not_found
  | (k',v) :: t when k' = k -> t, v
  | h :: t -> let t', v = pop k t in 
	      h :: t', v

let try_pop k l = 
  try let l, v = pop k l in l, Some v with Not_found -> l, None
  
let move ?after k l = 
  let l, v_opt = try_pop k l in
  match v_opt with None -> l | Some v ->
    match after with None -> (k,v) :: l | Some ki ->
      let rec aux = function
	| [] -> [k,v]
	| (k',_) as h :: t when k' = ki -> h :: (k,v) :: t
	| h :: t -> h :: aux t
      in
      aux l 

let keys   l = List.map fst l
let values l = List.map snd l
      
let rec map k f = function
  | [] -> []
  | (k',v) :: t when k = k' -> (k,f v) :: map k f t
  | h :: t -> h :: map k f t

let rec group_insert k v  = function
  | [] -> [k,[v]]
  | (k',l) :: t when k' = k -> (k,v::l) :: t
  | h :: t -> h :: group_insert k v t

let group list = 
  List.fold_left (fun acc (k,v) -> group_insert k v acc) [] list

let group_stable list = 
  List.rev (List.fold_left (fun acc (k,v) -> group_insert k v acc) [] (List.rev list))

