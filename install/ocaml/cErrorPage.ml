(* Ohm is © 2012 Victor Nicollet *)

open Ohm
open Ohm.Universal
open BatPervasives

let () = O.register_404 begin fun server path res ->
  let! title = ohm $ AdLib.get `ErrorPage_Error404_Title in 
  let! html  = ohm $ Asset_ErrorPage_Error404.render () in
  let  page  = Html.print_page ~css:[Asset.css] ~body_classes:["error-page"] ~title html in
  return $ Action.page page res
end
