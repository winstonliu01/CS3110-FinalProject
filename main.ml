open Deck
open Text
open Command
open Player
open Round

(*ToDo - Fix the deck in continuation. It is the same deck, we want a
  new deck each iteration.*)

let start_new_round deck player dealer =
  ANSITerminal.print_string [ ANSITerminal.red ] start_round_string;
  start_round deck player dealer

let rec yes_no (player : player) =
  let y_n = read_line () in
  match Command.check_yes_no y_n with
  | "yes" -> "yes"
  | "no" -> "no"
  | _ ->
      print_string "Invalid input, please try again.";
      print_string "> ";
      yes_no player

let continue_playing (player : player) (dealer : dealer) =
  print_endline "Would you like to play another round?";
  print_string "> ";
  let response = yes_no player in
  if response = "yes" then start_new_round init_deck player dealer_init
  else player

(** [main ()] starts blackjack. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] welcome_string;
  (*Printing starting text- dealer and player hands printed*)
  ANSITerminal.print_string [ ANSITerminal.red ] start_round_string;
  print_endline dealer_card1_string;
  let player = start_round init_deck player_init dealer_init in
  let player_cont =
    continue_playing (reset_player player) dealer_init
  in
  print_endline
    ( "Goodbye, you leave the game with "
    ^ string_of_int player_cont.chips
    ^ " chips." )

(* Execute the game engine. *)

let () = main ()
