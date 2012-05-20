module Json = struct
  type t = 
    | Null
    | Array of t list
    | Object of (string * t) list
    | Float of float
    | Int of int
    | Bool of bool
    | String of string	

  let to_string _ = "."

  exception Error of string
end

type json t = (A.t)
