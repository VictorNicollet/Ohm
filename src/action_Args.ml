type 'a cell = ('a -> string) * (string -> 'a option) 
type 'args t = ('args -> string list) * (string list -> 'args option)
  
let string = (fun str -> str), (fun str -> Some str) 
let int    = string_of_int, (fun i -> try Some (int_of_string i) with _ -> None)
  
let parse (_,f) list = f list
let generate (g,_) args = g args
  
let none = (fun () -> []), (function [] -> Some () | _ -> None) 
let i    = (fun ()  -> []), (fun _ -> Some ())
let n (f,g) = (fun l -> List.map f l), (fun l -> let l' = BatList.filter_map g l in
						 if List.length l' <> List.length l then None else Some l')

(* The source below was mass-generated. *)

let on (gen1, parse1) (genL, parseL) =
  (function
    | (Some x1,l) -> (gen1 x1) :: List.map genL l
    | (_,_) -> []
  ),
  (function
    | x1 :: l -> begin
      match parse1 x1 with None -> None | Some y1 ->
	let l' = BatList.filter_map parseL l in
	if List.length l' <> List.length l then None else
	  Some (Some y1,l')
    end
    | [] -> begin
      Some (None,[])
    end)
    
let oi (gen1, parse1) =
  (function
    | (Some x1) -> (gen1 x1) :: []
    | (_) -> []
  ),
  (function
    | x1 :: _ -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (Some y1)
    end
    | [] -> Some (None))
    
let o (gen1, parse1) =
  (function
    | (Some x1) -> (gen1 x1) :: []
    | (_) -> []
  ),
  (function
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (Some y1)
    end
    | [] -> Some (None)
    | _ -> None)
    
let oon (gen1, parse1) (gen2, parse2) (genL, parseL) =
  (function
    | (Some x1,Some x2,l) -> (gen1 x1) :: (gen2 x2) :: List.map genL l
    | (Some x1,_,_) -> (gen1 x1) :: []
    | (_,_,_) -> []
  ),
  (function
    | x1 :: x2 :: l -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  let l' = BatList.filter_map parseL l in
	  if List.length l' <> List.length l then None else
	    Some (Some y1,Some y2,l')
    end
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (Some y1,None,[])
    end
    | [] -> begin
      Some (None,None,[])
    end)
    
let ooi (gen1, parse1) (gen2, parse2) =
  (function
    | (Some x1,Some x2) -> (gen1 x1) :: (gen2 x2) :: []
    | (Some x1,_) -> (gen1 x1) :: []
    | (_,_) ->  []
  ),
  (function
    | x1 :: x2 :: _ -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (Some y1,Some y2)
    end
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (Some y1,None)
    end
    | [] -> begin
      Some (None,None)
    end)
    
let oo (gen1, parse1) (gen2, parse2) =
 (function
   | (Some x1,Some x2) -> (gen1 x1) :: (gen2 x2) :: []
   | (Some x1,_) -> (gen1 x1) :: []
   | (_,_) -> []
 ),
  (function
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (Some y1,Some y2)
    end
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (Some y1,None)
    end
    | [] -> begin
      Some (None,None)
    end
    | _ -> None)
    
let ooon (gen1, parse1) (gen2, parse2) (gen3, parse3) (genL, parseL) =
  (function
    | (Some x1,Some x2,Some x3,l) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: List.map genL l
    | (Some x1,Some x2,_,_) -> (gen1 x1) :: (gen2 x2) :: []
    | (Some x1,_,_,_) -> (gen1 x1) :: []
    | (_,_,_,_) -> []
  ),
  (function
    | x1 :: x2 :: x3 :: l -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    let l' = BatList.filter_map parseL l in
	    if List.length l' <> List.length l then None else
	      Some (Some y1,Some y2,Some y3,l')
    end
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (Some y1,Some y2,None,[])
    end
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (Some y1,None,None,[])
    end
    | [] -> begin
      Some (None,None,None,[])
    end)

let oooi (gen1, parse1) (gen2, parse2) (gen3, parse3) =
  (function
    | (Some x1,Some x2,Some x3) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
    | (Some x1,Some x2,_) -> (gen1 x1) :: (gen2 x2) :: []
    | (Some x1,_,_) -> (gen1 x1) :: []
    | (_,_,_) -> []
  ),
  (function
    | x1 :: x2 :: x3 :: _ -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (Some y1,Some y2,Some y3)
    end
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (Some y1,Some y2,None)
    end
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (Some y1,None,None)
    end
    | [] -> begin
      Some (None,None,None)
    end)
    
let ooo (gen1, parse1) (gen2, parse2) (gen3, parse3) =
  (function
    | (Some x1,Some x2,Some x3) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
    | (Some x1,Some x2,_) -> (gen1 x1) :: (gen2 x2) :: []
    | (Some x1,_,_) -> (gen1 x1) :: []
    | (_,_,_) -> []
  ),
  (function
    | x1 :: x2 :: x3 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (Some y1,Some y2,Some y3)
    end
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (Some y1,Some y2,None)
    end
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (Some y1,None,None)
    end
    | [] -> begin
      Some (None,None,None)
    end
    | _ -> None)
    
let oooon (gen1, parse1) (gen2, parse2) (gen3, parse3) (gen4, parse4) (genL, parseL) =
  (function
    | (Some x1,Some x2,Some x3,Some x4,l) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: (gen4 x4) :: List.map genL l
    | (Some x1,Some x2,Some x3,_,_) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
    | (Some x1,Some x2,_,_,_) -> (gen1 x1) :: (gen2 x2) :: []
    | (Some x1,_,_,_,_) -> (gen1 x1) :: []
    | (_,_,_,_,_) -> []
  ),
  (function
    | x1 :: x2 :: x3 :: x4 :: l -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    match parse4 x4 with None -> None | Some y4 ->
	      let l' = BatList.filter_map parseL l in
	      if List.length l' <> List.length l then None else
		Some (Some y1,Some y2,Some y3,Some y4,l')
    end
    | x1 :: x2 :: x3 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (Some y1,Some y2,Some y3,None,[])
    end
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (Some y1,Some y2,None,None,[])
    end
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (Some y1,None,None,None,[])
    end
    | [] -> begin
      Some (None,None,None,None,[])
    end)
    
let ooooi (gen1, parse1) (gen2, parse2) (gen3, parse3) (gen4, parse4) =
  (function
    | (Some x1,Some x2,Some x3,Some x4) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: (gen4 x4) :: []
    | (Some x1,Some x2,Some x3,_) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
    | (Some x1,Some x2,_,_) -> (gen1 x1) :: (gen2 x2) :: []
    | (Some x1,_,_,_) -> (gen1 x1) :: []
    | (_,_,_,_) -> []
  ),
  (function
    | x1 :: x2 :: x3 :: x4 :: _ -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    match parse4 x4 with None -> None | Some y4 ->
	      Some (Some y1,Some y2,Some y3,Some y4)
    end
    | x1 :: x2 :: x3 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (Some y1,Some y2,Some y3,None)
    end
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (Some y1,Some y2,None,None)
    end
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (Some y1,None,None,None)
    end
    | [] -> begin
      Some (None,None,None,None)
    end)
    
let oooo (gen1, parse1) (gen2, parse2) (gen3, parse3) (gen4, parse4) =
  (function
    | (Some x1,Some x2,Some x3,Some x4) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: (gen4 x4) :: []
    | (Some x1,Some x2,Some x3,_) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
    | (Some x1,Some x2,_,_) -> (gen1 x1) :: (gen2 x2) :: []
    | (Some x1,_,_,_) -> (gen1 x1) :: []
    | (_,_,_,_) -> []
  ),
  (function
    | x1 :: x2 :: x3 :: x4 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    match parse4 x4 with None -> None | Some y4 ->
	      Some (Some y1,Some y2,Some y3,Some y4)
    end
    | x1 :: x2 :: x3 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (Some y1,Some y2,Some y3,None)
    end
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (Some y1,Some y2,None,None)
    end
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (Some y1,None,None,None)
    end
    | [] -> begin
      Some (None,None,None,None)
    end
    | _ -> None)

let rn (gen1, parse1) (genL, parseL) =
  (function
    | (x1,l) -> (gen1 x1) :: List.map genL l
  ),
  (function
    | x1 :: l -> begin
      match parse1 x1 with None -> None | Some y1 ->
	let l' = BatList.filter_map parseL l in
	if List.length l' <> List.length l then None else
	  Some (y1,l')
    end
    | _ -> None)
    
let ri (gen1, parse1) =
  (function
    | (x1) -> (gen1 x1) :: []
  ),
  (function
    | x1 :: _ -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (y1)
    end
    | _ -> None)
    
let r (gen1, parse1) =
  (function
    | (x1) -> (gen1 x1) :: []
  ),
  (function
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (y1)
    end
    | _ -> None)
    
let ron (gen1, parse1) (gen2, parse2) (genL, parseL) =
  (function
    | (x1,Some x2,l) -> (gen1 x1) :: (gen2 x2) :: List.map genL l
    | (x1,_,_) -> (gen1 x1) :: []
  ),
  (function
    | x1 :: x2 :: l -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  let l' = BatList.filter_map parseL l in
	  if List.length l' <> List.length l then None else
	    Some (y1,Some y2,l')
    end
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (y1,None,[])
    end
    | _ -> None)
    
let roi (gen1, parse1) (gen2, parse2) =
  (function
    | (x1,Some x2) -> (gen1 x1) :: (gen2 x2) :: []
    | (x1,_) -> (gen1 x1) :: []
  ),
  (function
    | x1 :: x2 :: _ -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (y1,Some y2)
    end
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (y1,None)
    end
    | _ -> None)
    
let ro (gen1, parse1) (gen2, parse2) =
  (function
    | (x1,Some x2) -> (gen1 x1) :: (gen2 x2) :: []
    | (x1,_) -> (gen1 x1) :: []
  ),
  (function
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (y1,Some y2)
    end
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (y1,None)
    end
    | _ -> None)
    
let roon (gen1, parse1) (gen2, parse2) (gen3, parse3) (genL, parseL) =
  (function
    | (x1,Some x2,Some x3,l) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: List.map genL l
    | (x1,Some x2,_,_) -> (gen1 x1) :: (gen2 x2) :: []
    | (x1,_,_,_) -> (gen1 x1) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: l -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    let l' = BatList.filter_map parseL l in
	    if List.length l' <> List.length l then None else
	      Some (y1,Some y2,Some y3,l')
    end
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (y1,Some y2,None,[])
    end
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (y1,None,None,[])
    end
    | _ -> None)
    
let rooi (gen1, parse1) (gen2, parse2) (gen3, parse3) =
  (function
    | (x1,Some x2,Some x3) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
    | (x1,Some x2,_) -> (gen1 x1) :: (gen2 x2) :: []
    | (x1,_,_) -> (gen1 x1) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: _ -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (y1,Some y2,Some y3)
    end
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (y1,Some y2,None)
    end
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (y1,None,None)
    end
    | _ -> None)
    
let roo (gen1, parse1) (gen2, parse2) (gen3, parse3) =
  (function
    | (x1,Some x2,Some x3) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
    | (x1,Some x2,_) -> (gen1 x1) :: (gen2 x2) :: []
    | (x1,_,_) -> (gen1 x1) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (y1,Some y2,Some y3)
    end
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (y1,Some y2,None)
    end
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (y1,None,None)
    end
    | _ -> None)
    
let rooon (gen1, parse1) (gen2, parse2) (gen3, parse3) (gen4, parse4) (genL, parseL) =
  (function
    | (x1,Some x2,Some x3,Some x4,l) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: (gen4 x4) :: List.map genL l
    | (x1,Some x2,Some x3,_,_) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
    | (x1,Some x2,_,_,_) -> (gen1 x1) :: (gen2 x2) :: []
    | (x1,_,_,_,_) -> (gen1 x1) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: x4 :: l -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    match parse4 x4 with None -> None | Some y4 ->
	      let l' = BatList.filter_map parseL l in
	      if List.length l' <> List.length l then None else
		Some (y1,Some y2,Some y3,Some y4,l')
    end
    | x1 :: x2 :: x3 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (y1,Some y2,Some y3,None,[])
    end
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (y1,Some y2,None,None,[])
    end
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (y1,None,None,None,[])
    end
    | _ -> None)
    
let roooi (gen1, parse1) (gen2, parse2) (gen3, parse3) (gen4, parse4) =
  (function
    | (x1,Some x2,Some x3,Some x4) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: (gen4 x4) :: []
    | (x1,Some x2,Some x3,_) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
    | (x1,Some x2,_,_) -> (gen1 x1) :: (gen2 x2) :: []
    | (x1,_,_,_) -> (gen1 x1) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: x4 :: _ -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    match parse4 x4 with None -> None | Some y4 ->
	      Some (y1,Some y2,Some y3,Some y4)
    end
    | x1 :: x2 :: x3 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (y1,Some y2,Some y3,None)
    end
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (y1,Some y2,None,None)
    end
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (y1,None,None,None)
    end
    | _ -> None)
    
let rooo (gen1, parse1) (gen2, parse2) (gen3, parse3) (gen4, parse4) =
  (function
    | (x1,Some x2,Some x3,Some x4) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: (gen4 x4) :: []
    | (x1,Some x2,Some x3,_) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
    | (x1,Some x2,_,_) -> (gen1 x1) :: (gen2 x2) :: []
    | (x1,_,_,_) -> (gen1 x1) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: x4 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    match parse4 x4 with None -> None | Some y4 ->
	      Some (y1,Some y2,Some y3,Some y4)
    end
    | x1 :: x2 :: x3 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (y1,Some y2,Some y3,None)
    end
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (y1,Some y2,None,None)
    end
    | x1 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	Some (y1,None,None,None)
    end
    | _ -> None)
    
let rrn (gen1, parse1) (gen2, parse2) (genL, parseL) =
  (function
    | (x1,x2,l) -> (gen1 x1) :: (gen2 x2) :: List.map genL l
  ),
  (function
    | x1 :: x2 :: l -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  let l' = BatList.filter_map parseL l in
	  if List.length l' <> List.length l then None else
	    Some (y1,y2,l')
    end
    | _ -> None)
    
let rri (gen1, parse1) (gen2, parse2) =
  (function
    | (x1,x2) -> (gen1 x1) :: (gen2 x2) :: []
  ),
  (function
    | x1 :: x2 :: _ -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (y1,y2)
    end
    | _ -> None)
    
let rr (gen1, parse1) (gen2, parse2) =
  (function
    | (x1,x2) -> (gen1 x1) :: (gen2 x2) :: []
  ),
  (function
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (y1,y2)
    end
    | _ -> None)
    
let rron (gen1, parse1) (gen2, parse2) (gen3, parse3) (genL, parseL) =
  (function
    | (x1,x2,Some x3,l) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: List.map genL l
    | (x1,x2,_,_) -> (gen1 x1) :: (gen2 x2) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: l -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    let l' = BatList.filter_map parseL l in
	    if List.length l' <> List.length l then None else
	      Some (y1,y2,Some y3,l')
    end
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (y1,y2,None,[])
    end
    | _ -> None)
    
let rroi (gen1, parse1) (gen2, parse2) (gen3, parse3) =
  (function
    | (x1,x2,Some x3) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
    | (x1,x2,_) -> (gen1 x1) :: (gen2 x2) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: _ -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (y1,y2,Some y3)
    end
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (y1,y2,None)
    end
    | _ -> None)
    
let rro (gen1, parse1) (gen2, parse2) (gen3, parse3) =
  (function
    | (x1,x2,Some x3) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
    | (x1,x2,_) -> (gen1 x1) :: (gen2 x2) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (y1,y2,Some y3)
    end
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (y1,y2,None)
    end
    | _ -> None)
    
let rroon (gen1, parse1) (gen2, parse2) (gen3, parse3) (gen4, parse4) (genL, parseL) =
  (function
    | (x1,x2,Some x3,Some x4,l) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: (gen4 x4) :: List.map genL l
    | (x1,x2,Some x3,_,_) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
    | (x1,x2,_,_,_) -> (gen1 x1) :: (gen2 x2) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: x4 :: l -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    match parse4 x4 with None -> None | Some y4 ->
	      let l' = BatList.filter_map parseL l in
	      if List.length l' <> List.length l then None else
		Some (y1,y2,Some y3,Some y4,l')
    end
    | x1 :: x2 :: x3 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (y1,y2,Some y3,None,[])
    end
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (y1,y2,None,None,[])
    end
    | _ -> None)
    
let rrooi (gen1, parse1) (gen2, parse2) (gen3, parse3) (gen4, parse4) =
  (function
    | (x1,x2,Some x3,Some x4) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: (gen4 x4) :: []
    | (x1,x2,Some x3,_) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
    | (x1,x2,_,_) -> (gen1 x1) :: (gen2 x2) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: x4 :: _ -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    match parse4 x4 with None -> None | Some y4 ->
	      Some (y1,y2,Some y3,Some y4)
    end
    | x1 :: x2 :: x3 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (y1,y2,Some y3,None)
    end
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (y1,y2,None,None)
    end
    | _ -> None)
    
let rroo (gen1, parse1) (gen2, parse2) (gen3, parse3) (gen4, parse4) =
  (function
    | (x1,x2,Some x3,Some x4) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: (gen4 x4) :: []
    | (x1,x2,Some x3,_) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
    | (x1,x2,_,_) -> (gen1 x1) :: (gen2 x2) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: x4 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    match parse4 x4 with None -> None | Some y4 ->
	      Some (y1,y2,Some y3,Some y4)
    end
    | x1 :: x2 :: x3 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (y1,y2,Some y3,None)
    end
    | x1 :: x2 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  Some (y1,y2,None,None)
    end
    | _ -> None)
    
let rrrn ((gen1, parse1):'a cell) ((gen2, parse2):'b cell) ((gen3, parse3):'c cell) ((genL, parseL):'l cell) =
  (function
    | (x1,x2,x3,l) -> ((gen1 x1) :: (gen2 x2) :: (gen3 x3) :: List.map genL l : string list)
  ),
  (function
    | x1 :: x2 :: x3 :: l -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    let l' = BatList.filter_map parseL l in
	    if List.length l' <> List.length l then None else
	      Some (y1,y2,y3,l')
    end
    | (_:string list) -> None)
    
let rrri (gen1, parse1) (gen2, parse2) (gen3, parse3) =
  (function
    | (x1,x2,x3) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: _ -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (y1,y2,y3)
    end
    | _ -> None)
    
let rrr (gen1, parse1) (gen2, parse2) (gen3, parse3) =
  (function
    | (x1,x2,x3) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (y1,y2,y3)
    end
    | _ -> None)
    
let rrron (gen1, parse1) (gen2, parse2) (gen3, parse3) (gen4, parse4) (genL, parseL) =
  (function
    | (x1,x2,x3,Some x4,l) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: (gen4 x4) :: List.map genL l
    | (x1,x2,x3,_,_) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: x4 :: l -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    match parse4 x4 with None -> None | Some y4 ->
	      let l' = BatList.filter_map parseL l in
	      if List.length l' <> List.length l then None else
		Some (y1,y2,y3,Some y4,l')
    end
    | x1 :: x2 :: x3 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (y1,y2,y3,None,[])
    end
    | _ -> None)
    
let rrroi (gen1, parse1) (gen2, parse2) (gen3, parse3) (gen4, parse4) =
  (function
    | (x1,x2,x3,Some x4) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: (gen4 x4) :: []
    | (x1,x2,x3,_) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: x4 :: _ -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    match parse4 x4 with None -> None | Some y4 ->
	      Some (y1,y2,y3,Some y4)
    end
    | x1 :: x2 :: x3 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (y1,y2,y3,None)
    end
    | _ -> None)
    
let rrro (gen1, parse1) (gen2, parse2) (gen3, parse3) (gen4, parse4) =
  (function
    | (x1,x2,x3,Some x4) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: (gen4 x4) :: []
    | (x1,x2,x3,_) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: x4 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    match parse4 x4 with None -> None | Some y4 ->
	      Some (y1,y2,y3,Some y4)
    end
    | x1 :: x2 :: x3 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    Some (y1,y2,y3,None)
    end
    | _ -> None)
    
let rrrrn (gen1, parse1) (gen2, parse2) (gen3, parse3) (gen4, parse4) (genL, parseL) =
  (function
    | (x1,x2,x3,x4,l) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: (gen4 x4) :: List.map genL l
  ),
  (function
    | x1 :: x2 :: x3 :: x4 :: l -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    match parse4 x4 with None -> None | Some y4 ->
	      let l' = BatList.filter_map parseL l in
	      if List.length l' <> List.length l then None else
		Some (y1,y2,y3,y4,l')
    end
    | _ -> None)
    
let rrrri (gen1, parse1) (gen2, parse2) (gen3, parse3) (gen4, parse4) =
  (function
    | (x1,x2,x3,x4) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: (gen4 x4) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: x4 :: _ -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    match parse4 x4 with None -> None | Some y4 ->
	      Some (y1,y2,y3,y4)
    end
    | _ -> None)
    
let rrrr (gen1, parse1) (gen2, parse2) (gen3, parse3) (gen4, parse4) =
  (function
    | (x1,x2,x3,x4) -> (gen1 x1) :: (gen2 x2) :: (gen3 x3) :: (gen4 x4) :: []
  ),
  (function
    | x1 :: x2 :: x3 :: x4 :: [] -> begin
      match parse1 x1 with None -> None | Some y1 ->
	match parse2 x2 with None -> None | Some y2 ->
	  match parse3 x3 with None -> None | Some y3 ->
	    match parse4 x4 with None -> None | Some y4 ->
	      Some (y1,y2,y3,y4)
    end
    | _ -> None)
    
(* Generator code (requires some manual adjustments after generation *)

module Gen = struct

  let list_init f n = 
    let rec aux i = 
      if i = n then [] else f i :: aux (i+1)
    in aux 0
    
  let nth i = string_of_int (succ i)
    
  let cell i = "(gen" ^ nth i ^ ", parse" ^ nth i ^ ")" 
    
  let gen r o kind = 
    
    let list = match kind with 
      | `List   -> " (genL, parseL)"
      | `Ignore -> ""
      | `None   -> ""
    in
    
    let tail = match kind with 
      | `List   -> "List.map genL l"
      | `Ignore -> "[]"
      | `None   -> "[]"
    in
    
    let tail' = match kind with 
      | `List -> "l"
      | `Ignore -> "_" 
      | `None -> "[]"
    in
    
    let suffix = match kind with 
      | `List -> "n"
      | `Ignore -> "i"
      | `None -> ""
    in
    
    Printf.sprintf "let %s%s%s %s%s =\n (function%s\n),\n(function%s%s)\n"
      (String.make r 'r')
      (String.make o 'o') 
      suffix
      (String.concat " " (list_init cell (r + o)))
      list
      
      begin String.concat "" (list_init (fun o' -> 
	let o' = o - o' in
	Printf.sprintf "\n| (%s%s) -> %s :: %s"	
	  
	  (String.concat "," (list_init (fun i -> 
	    if i < r then "x" ^ nth i else
	      if i < r + o' then "Some x" ^ nth i else "_") (r+o)))
	  
	  (if kind = `List then
	      if o <> o' then ",_" else ",l"
	   else "")
	  
	  (String.concat " :: " (list_init (fun i ->
	    "(gen" ^ nth i ^ " x" ^ nth i ^ ")" ) (r + o' )))
	  
	  (if o <> o' then "[]" else tail)
	  
      ) (o + 1)) end
      
      begin String.concat "" (list_init (fun o' ->
	let o' = o - o' in
	Printf.sprintf "\n| %s :: %s -> begin\n%s%s Some (%s%s)\nend"	
	  
	  (String.concat " :: " (list_init (fun i ->
	    "x"^nth i) (r + o')))
	  
	  (if o <> o' then "[]" else tail') 
	  
	  (String.concat " " (list_init (fun i ->
	    "match parse" ^ nth i ^ " x" ^ nth i 
	    ^ " with None -> None | Some y" ^ nth i ^ " ->\n") (r + o'))) 
	  
	  (if kind = `List then
	      if o <> o' then "" else 
		" let l' = BatList.filter_map parseL l in\nif List.length l' <> List.length l then None else\n"
	   else
	      "")
	  
	  (String.concat "," (list_init (fun i ->
	    if i < r then "y" ^ nth i else
	      if i < r + o' then "Some y" ^ nth i else
		"None") (r + o)))
	  
	  (if kind = `List then 
	      if o <> o' then ",[]" else ",l'"
	   else "")
	  
      ) (o + 1)) end 
      
      "\n| _ -> None"
      
  let print () = 
    for r = 0 to 4 do
      for o = (if r = 0 then 1 else 0) to 4 - r do
	print_endline (gen r o `List) ;
	print_endline (gen r o `Ignore) ;
	print_endline (gen r o `None)
      done
    done


end
