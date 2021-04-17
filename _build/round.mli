open Text
open Deck
open Player

val print_hand : card list -> unit

val card : deck -> unit

val parse_input : deck -> int -> unit

val command : deck -> int -> unit

val start_round : deck -> player -> dealer -> unit

val init_deck : deck
