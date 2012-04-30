(* Ohm is © 2012 Victor Nicollet *)

module ImplTable = CouchDB_table
module ImplTypes = CouchDB_types

module type LOCAL_CONFIG = sig
  val db : string
end

module Config = functor (C:LOCAL_CONFIG) -> struct
  let host     = "localhost"
  let port     = 5984
  let database = C.db
end

module Database = functor(C:LOCAL_CONFIG) -> struct
  module C = Config(C)
  include ImplTable.Database(C)
end

module Table = 
  functor(C:LOCAL_CONFIG) -> 
    functor (Id:ImplTypes.ID) -> 
      functor (Type:Fmt.FMT) -> 
struct
  module Db = Database(C)
  module MyTable = ImplTable.Table(Db)(Id)(Type)
  module Design = struct
    module Database = Db
    let name = C.db
  end
end
