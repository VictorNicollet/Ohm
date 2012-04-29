(* Ohm is © 2012 Victor Nicollet *)

(* Types, both public and private ---------------------------------------------------------- *)

type thread          = Do of thread list Lazy.t
type ('ctx,'value) t = 'ctx -> ('value -> thread) -> thread

type ('ctx,'value) value  = ('ctx,'value) t
type 'ctx          effect = ('ctx,unit) t

(* Monad usage ----------------------------------------------------------------------------- *)

let return x = fun _ emit -> Do (lazy [emit x])   
let bind f m = fun c emit -> m c (fun x -> f x c emit)
let map  f m = fun c emit -> m c (fun x -> emit (f x))
let unwrap m = fun c emit -> m c (fun x -> x c emit) 

(* Context manipulation -------------------------------------------------------------------- *)

let context = fun c emit -> Do (lazy [emit c]) 

let with_context c m = fun _ emit -> m c emit

let edit_context f m = fun c emit -> m (f c) emit

(* Concurrency manipulation ---------------------------------------------------------------- *)

let nop = Do (lazy [])

let yield m = fun c emit -> Do (lazy [nop ; m c emit]) 

let join  a b f = fun c emit -> let ra = ref None and rb = ref None in 
				let emit_a xa = match !rb with 
				  | None    -> ra := Some xa ; nop
				  | Some xb -> f xa xb c emit
				and emit_b xb = match !ra with
				  | None    -> rb := Some xb ; nop
				  | Some xa -> f xa xb c emit
				in
				Do (lazy [a c emit_a ; b c emit_b]) 

let fork a b = fun c emit -> Do (lazy [b c emit ; a c (fun _ -> nop)])

(* Utilities ------------------------------------------------------------------------------ *)

let memo m = 
  let r = ref None in 
  fun c emit -> Do (lazy [(
    match !r with 
      | Some (c',v) when c' == c -> emit v 
      | _ -> m c (fun x -> r := Some (c,x) ; emit x) 
  )])

let of_lazy l = fun c emit -> Do (lazy [emit (Lazy.force l)])
let of_func f = fun c emit -> Do (lazy [emit (f ())])

let of_call f a = fun c emit -> f a c emit

let list_map f l = fun c emit -> 
  if l = [] then emit [] else 
    let num_unevaled = ref (List.length l) in
    let result = List.map (fun x -> x, ref None) l in
    let emit r y = 
      if !r = None then decr num_unevaled ; 
      r := Some y ;
      if !num_unevaled > 0 then nop 
      else emit (List.map 
		   (fun (x,r) -> match !r with Some y -> y | None -> assert false) 
		   result)
    in
    Do (lazy (List.map (fun (x,r) -> f x c (emit r)) result))

let list_filter  f l = map (BatList.filter_map BatPervasives.identity) (list_map f l)
let list_collect f l = map List.concat (list_map f l)

let rec list_find f = function 
  | []     -> return None
  | h :: t -> bind (function 
      | None -> list_find f t
      | some -> return some) (f h) 

let rec list_fold f a = function
  | []     -> return a
  | h :: t -> bind (fun a -> list_fold f a t) (f h a) 

let list_mfold f a l = bind (fun l -> list_fold (fun f a -> f a) a l) (list_map f l) 
		       
let list_iter f l = fun c emit -> 
  if l = [] then emit () else 
    let r = ref (List.length l) in
    let emit () = 
      decr r ; 
      if !r = 0 then emit () else nop 
    in
    Do (lazy (List.map (fun x -> f x c emit) l))
    
let list_exists pred l = 
  map (function None -> false | Some () -> true)
    (list_find (fun x -> map (fun px -> if px then Some () else None) (pred x)) l)

let opt_map f = function 
  | None   -> (fun c emit -> emit None)
  | Some x -> (fun c emit -> f x c (fun y -> emit (Some y)))

let opt_bind f = function
  | None   -> (fun c emit -> emit None)
  | Some x -> (fun c emit -> f x c emit)

(* Evaluation ------------------------------------------------------------------------------ *)

let eval ctx m = 
  let queue  = Queue.create () in 
  let r      = ref None in 
  let emit x = r := Some x ; nop in 
  
  let rec loop = function Do step -> 
    match Lazy.force step with 
      | h :: t -> List.iter (fun x -> Queue.push x queue) t ; loop h
      | []     -> match try Some (Queue.pop queue) with Queue.Empty -> None with
	  | Some thread -> loop thread
	  | None        -> () 
  in

  loop (m ctx emit) ;
  match !r with None -> assert false | Some result -> result
