(**Functions needed to operate a game of Blackjack*)

open Text
open Deck
open Player

(**[init_deck] is the deck when the game begins*)
val init_deck : deck

(**[card] prints out the first card in the deck*)
val card : deck -> unit

(**[parse_input] determines whether the player wants to hit or stay and
   will update the player state*)
val parse_input : deck -> player -> dealer -> player -> player * player

(** [start_round] is the player state after a round is over *)
val start_round :
  deck -> player -> dealer -> player -> string -> player * player
