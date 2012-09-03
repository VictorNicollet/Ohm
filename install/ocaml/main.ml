(* © 2012 RunOrg *) 

open Ohm
open BatPervasives

module Main = Ohm.Main.Make(O.Reset)
let _ = Main.run ~async:O.run_async O.role


