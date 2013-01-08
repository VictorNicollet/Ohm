(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives

open Common 

let root = Sys.getcwd () 
  
let ocaml   = Filename.concat root  "ocaml"
let plugins = Filename.concat ocaml "plugins"
let ohm     = Filename.concat ocaml "ohm"
let www     = Filename.concat root  "www"
let bot     = Filename.concat root  ".bot"
let gen     = Filename.concat ocaml "gen"
let assets  = Filename.concat root  "assets" 
let build   = Filename.concat root  "_build"
let public  = Filename.concat root  "public"
  
let less    = Filename.concat build "style.less"
let css     = Filename.concat build "style.css"
let css_url = "/style.css"    
  
let coffee  = Filename.concat build "script.coffee"
let js      = Filename.concat build "script.js"
let js_url  = "/script.js"    
  
let assetml = Filename.concat build "asset.ml"
  
let jsml    = Filename.concat build "js.ml"
let jsmli   = Filename.concat build "js.mli"

let install = Filename.concat root ".ohm"
let plugsrc = Filename.concat install "Ohm-Plugins"
