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

module A = struct
  module B = struct
    type json t = int
    let to_json = json_of_t
    let of_json = t_of_json 
  end
end

type json t = { 
  ab : A.B.t ;
  a : int ; 
  ?b : string option ;
  ?c : int = (3+3)
}


