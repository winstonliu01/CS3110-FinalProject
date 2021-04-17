open Deck
open Text
open Command
open Player
open Round

(** [main ()] starts blackjack. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] welcome_string;

  start_round init_deck player_init dealer_init

(* Execute the game engine. *)

let () = main ()
