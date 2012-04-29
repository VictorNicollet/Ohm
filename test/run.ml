(* Test the "Ohm.Run" module *)

open BatPervasives

open Ohm.Universal
module R = Ohm.Run

(* Helper function for testing sequence... *)
let mkseq () = let s = ref "" in 
	       (fun c v -> R.of_func (fun () -> s := !s ^ c ; v)),
	       (fun e -> if !s = e then true else (print_endline !s ; false ))

(* Testing basic functions and context. ----------------------------------------------------- *)
let _ = 
  assert (R.eval () (R.return 10) = 10) ;
  assert (R.eval 42 R.context = 42) ;
  assert (R.eval 13 (R.with_context 50 R.context) = 50) ;
  assert (R.eval 13 (let! c = ohm R.context in R.with_context (c + 10) R.context) = 23) ;
  assert (R.eval 28 (R.map string_of_int R.context) = "28") ;
  assert (R.eval () (R.unwrap (R.return (R.return true))) = true) 

(* Test various sequencing tricks. ---------------------------------------------------------- *)
let _ = 
  let say, is = mkseq () in
  let () = R.eval () (let! () = ohm $ say "a" () in
		      let! () = ohm $ say "b" () in
		      let! () = ohm $ say "c" () in R.return ()) in
    
  assert (is "abc")

let _ = 
  let say, is = mkseq () in
  let r = R.eval () (R.join (say "a" 5) (say "b" 6) (fun a b -> say "c" (a+b))) in
  assert (r = 11) ;
  assert (is "abc")

let _ = 
  let say, is = mkseq () in
  let r = R.eval () (R.join 
		       (R.join (say "a" 1) (say "b" 2) (fun a b -> say "c" (a+b)))
		       (R.join (say "d" 3) (say "e" 4) (fun a b -> say "f" (a+b)))
		       (fun a b -> say "g" (a+b))) in
  assert (r = 10) ;
  assert (is "adbcefg") 

let _ = 
  let say, is = mkseq () in
  let r = R.eval () (R.join (R.yield (say "a" 9)) (say "b" 4) (fun a b -> say "c" (a-b))) in
  assert (r = 5) ;
  assert (is "bac")

let _ = 
  let say, is = mkseq () in
  let () = R.eval () (R.fork (say "a" ()) (say "b" ())) in
  assert (is "ba") 

let _ = 
  let say, is = mkseq () in
  let () = R.eval () (R.fork (say "a" ()) (R.yield (say "b" ()))) in
  assert (is "ab") 

let _ = 
  let say, is = mkseq () in
  let () = R.eval () (let! () = fork (say "a" ()) in say "b" ())in
  assert (is "ba") 

let _ = 
  let say, is = mkseq () in
  let () = R.eval () (let! () = fork (say "b" ()) in 
		      let! () = fork (say "c" ()) in
		      say "a" ()) in
  assert (is "abc") 

(* Memoization test. ----------------------------------------------------------------------- *)

let _ = 
  let say, is = mkseq () in
  let unstored  = say "a" 5 in
  assert (R.eval () (R.join unstored unstored (fun a b -> R.return (a+b))) = 10) ;
  assert (is "aa")

let _ = 
  let say, is = mkseq () in
  let stored  = R.memo (say "a" 5) in
  assert (R.eval 0 (R.join stored stored (fun a b -> R.return (a+b))) = 10) ;
  assert (is "a")

let _ = 
  let say, is = mkseq () in
  let stored  = R.memo (say "a" 5) in
  assert (R.eval 1 (R.join stored stored (fun a b -> R.return (a+b))) = 10) ;
  assert (R.eval 2 (R.join stored stored (fun a b -> R.return (a+b))) = 10) ;
  assert (is "aa")

(* List processing tests. ------------------------------------------------------------------ *)

let _ = 
  let say, is = mkseq () in
  let list = R.eval () $ R.list_map (fun (s,i) -> say s i) ["a",1;"b",2;"c",3;"d",4;"e",5] in
  assert (list = [1;2;3;4;5]) ;
  assert (is "abcde")

let _ = 
  let say, is = mkseq () in
  let list = R.eval () $ R.list_filter 
    (fun (s,i) -> say s (if i mod 2 = 0 then Some (i/2) else None)) 
      ["a",1;"b",2;"c",3;"d",4;"e",5] in
  assert (list = [1;2]) ;
  assert (is "abcde")

let _ = 
  let say, is = mkseq () in
  let () = R.eval () $ R.list_iter (fun (s,i) -> say s ()) ["a",1;"b",2;"c",3;"d",4;"e",5] in
  assert (is "abcde")

let _ = 
  let say, is = mkseq () in
  let item = R.eval () $ R.list_find
    (fun (s,i) -> say s (if i mod 2 = 0 then Some (i/2) else None)) 
      ["a",1;"b",2;"c",3;"d",4;"e",5] in
  assert (item = Some 1) ;
  assert (is "ab")

let _ = 
  let say, is = mkseq () in
  let list = R.eval () $ R.list_collect
    (fun (s,i) -> say s [i;i*2])
      ["a",1;"b",2;"c",3;"d",4;"e",5] in
  assert (list = [1;2;2;4;3;6;4;8;5;10]) ;
  assert (is "abcde")

let _ = 
  assert ([] = R.eval () (R.list_map (fun () -> R.return ()) []))

let _ = 
  assert (() = R.eval () (R.list_iter (fun () -> R.return ()) []))
