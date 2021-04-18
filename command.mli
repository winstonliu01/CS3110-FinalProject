type command = string

exception Empty

exception Malformed

(** [check_string] determines what to do based on the command*)
val check_string : string -> string

(** [check_yn] determines whether to start a new round*)
val check_yn : string -> string
