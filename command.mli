type command = string

(** [check_1_11] determines whether an ace should be worth 1 or 11
    points*)
val check_1_11 : string -> string

(** [check_hit_stay] determines what to do based on the command*)
val check_hit_stay : string -> string

(** [check_yes_no] determines whether to start a new round*)
val check_yes_no : string -> string
