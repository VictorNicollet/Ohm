(* © 2012 RunOrg *) 

open Ohm
open BatPervasives

module Main = Ohm.Main.Make(O.Reset)
let _ = Main.run (Some O.run_async)


