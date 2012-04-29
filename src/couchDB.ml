(* Ohm is © 2012 Victor Nicollet *)

open BatPervasives

module ImplDB = CouchDB_database
module ImplCache = CouchDB_cache
module ImplTypes = CouchDB_types
module ImplTable = CouchDB_table
module ImplViews = CouchDB_views

(* Database-related definitions ------------------------------------------------------------ *)

type database = ImplDB.t = {
  db_id     : int ;
  db_host   : string ;
  db_port   : int ;
  db_name   : string ;
  db_prefix : string
}

let compact = ImplDB.compact

(* Cache-related definitions --------------------------------------------------------------- *)

exception CouchDB_Error = ImplCache.CouchDB_error
type implementation = ImplCache.t

(* Type-related definitions ---------------------------------------------------------------- *)

module Parser = CouchDB_parser
include ImplTypes

let ctx_decay (ctx : #ctx) = (ctx :> ctx)

class init_ctx = object
  val time = Unix.gettimeofday ()
  method time = time
  inherit ctx
end

(* Database and table definitions ---------------------------------------------------------- *)

module Database  = ImplTable.Database
module ReadTable = ImplTable.ReadTable
module Table     = ImplTable.Table

(* View-related definitions ---------------------------------------------------------------- *)

module ReduceView = ImplViews.ReduceView
module DocView    = ImplViews.DocView
module MapView    = ImplViews.MapView

let compile_views () = ImplViews.compile_views ()
