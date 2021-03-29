type command = string

exception Empty

exception Malformed

(** [check_string] determines what to do based on the command*)
val check_string : string -> string
