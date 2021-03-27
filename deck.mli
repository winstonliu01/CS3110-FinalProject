(** Representation of standard 52 card deck *)

(** abstract type representing a deck *)
type deck

(** abstract type representing cards *)
type t

(** [create] is a deck of 52 cards that are not shuffled*)
val create : t list

(** [shuffle deck] is [deck] shuffled randomly *)
val shuffle : t list -> t list

(** [draw deck] is the first card picked from [deck] *)
val draw : t list -> t

(** [print_card card] is [card] displayed in our string format *)
val print_card : t -> string list
