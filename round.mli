open Text
open Deck
open Player

(*[card] prints out the first card in the deck*)
val card : deck -> unit

(*[parse_input] prints depending on whether a hit or stay is inputted*)
val parse_input : deck -> player -> dealer -> unit

(* [start_round] is the state of the game when a new round begins *)
val start_round : deck -> player -> dealer -> unit

(*[init_deck] is the deck when the game begins*)
val init_deck : deck
