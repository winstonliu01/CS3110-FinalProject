(** Parses command to check for valid input*)

type command = string

(** [check_1_11] determines whether the user wants the ace to be 1 or 11
    points*)
val check_1_11 : string -> string

(** [check_hit_stay] determines whether to draw another card or
    calculate the user's hand value*)
val check_hit_stay : string -> string

(** [check_yes_no] determines whether the user wants to start a new
    round*)
val check_yes_no : string -> string

(** [check_bet] determines whether the user entered a valid bet *)
val check_bet : string -> string

(** [check_side_bet] determines whether the user entered a valid side
    bet *)
val check_side_bet : string -> string
