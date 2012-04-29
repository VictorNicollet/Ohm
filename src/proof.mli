module type KEY = sig
  val key : string
end

module type PROOF = sig

  val prove    : string list -> string
  val is_proof : string -> string list -> bool

  val passhash : string -> string

end

module Make : functor (Key:KEY) -> PROOF
