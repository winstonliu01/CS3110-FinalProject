(** Representation of standard 52 card deck *)

(** abstract type representing a deck *)
type deck

(** abstract type representing cards *)
type card

(** [create] is a deck of 52 cards that are not shuffled*)
val create : deck

(** [shuffle deck] is [deck] shuffled randomly *)
val shuffle : deck -> deck

(** [draw deck] is the first card picked from [deck] *)
val draw : deck -> card

(** [remove deck] is [deck] with its first element removed *)
val remove : deck -> deck

(** [empty] is the truth value if [deck] has any cards*)
val empty : deck -> bool

(** [sort_deck] is [deck] sorted*)
val sort_deck : deck -> deck

(** [print_card card] is [card] displayed in our string format *)
val print_card : card -> unit
