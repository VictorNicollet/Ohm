(* Ohm is © 2012 Victor Nicollet *)

open Util
open BatPervasives

module BS = BatString

let path_clean path = 
  if BS.is_empty path then path 
  else let path = 
	 if BS.ends_with path "/" then BS.rchop path 
	 else path 
       in if BS.starts_with path "/" then BS.lchop path
	 else path
