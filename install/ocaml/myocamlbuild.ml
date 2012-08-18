open Ocamlbuild_plugin

let path_to_pp = "../ohm/pp.cmo"

let _ = dispatch begin function
  | After_rules ->
    flag ["ocamldep"; "custom-pp"] (S[A"-ppopt";A path_to_pp]);
    flag ["compile"; "custom-pp"] (S[A"-ppopt";A path_to_pp]);
  | _ -> ()
end

