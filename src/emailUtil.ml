(* Ohm is © 2011 Victor Nicollet *)

let canonical email_candidate = 

  (* All e-mails should be processed as lowercase anyway. *)
  let canonical_candidate = String.lowercase email_candidate in

  (* This regular expression is perhaps not the most adapted one, but it should
     do the trick in the vast majority of cases. *)
  let regexp = Str.regexp begin
    "[a-z0-9!#$%&'*+/=?^_`{|}~-]+\\(\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+\\)*"
    ^ "@"
    ^ "\\([a-z0-9]\\([a-z0-9-]*[a-z0-9]\\)?\\.\\)+[a-z0-9]\\([a-z0-9-]*[a-z0-9]\\)"
  end in 

  (* Attempt to extract the e-mail. If no regexp match is found, assume entire 
     string is an e-mail (after all, who knows). *)
  let extracted = 
    try let _ = Str.search_forward regexp canonical_candidate 0 in
	Str.matched_string canonical_candidate
    with Not_found -> canonical_candidate 
  in

  extracted
