(** Representation of standard 52 card deck *)

(** type representing a deck *)
type deck

(** type representing cards *)
type card = {
  suite : string;
  rank : string;
  point : int list;
}

(** [create] is a deck of 52 cards that are not shuffled*)
val create : deck

(** [shuffle deck] is [deck] shuffled randomly *)
val shuffle : deck -> deck

(** [draw deck] is the first card picked from [deck] *)
val draw : deck -> card

(** [remove deck] is [deck] with its first element removed *)
val remove : deck -> deck

(** [size] is the amount of cards in [deck]*)
val size : deck -> int

(** [empty] is the truth value if [deck] has any cards*)
val empty : deck -> bool

(** [point] is the point value of the card*)
val point : card -> int list

(** [sort_deck] is [deck] sorted*)
val sort_deck : deck -> deck

(** [print_card card] is [card] displayed in our string format *)
val print_card : card -> unit

(**[card] prints out the first card in the deck*)
val card : deck -> unit
