open Deck
open Text
open Command
open Player
open Round

(*ToDo - Fix the deck in continuation. It is the same deck, we want a
  new deck each iteration. Make it so that you can play more than 2
  rounds.*)

let start_new_round deck player dealer =
  ANSITerminal.print_string [ ANSITerminal.red ] start_round_string;
  start_round deck player dealer

let rec yes_no (player : player) =
  let y_n = read_line () in
  match Command.check_yes_no y_n with
  | "yes" -> "yes"
  | "no" -> "no"
  | "empty" ->
      print_endline "\nEmpty input, please try again. \n";
      print_string "> ";
      yes_no player
  | _ ->
      print_endline "Invalid input, please try again.";
      print_string "> ";
      yes_no player

let rec continue_playing (player : player) (dealer : dealer) =
  print_endline "Would you like to play another round?";
  print_string "> ";
  let response = yes_no player in
  if response = "yes" then (
    let new_player =
      print_endline dealer_card1_string;
      start_new_round (shuffle init_deck) player dealer_init
    in
    if new_player.is_blackjack = true then
      print_endline "You got Blackjack"
    else ();
    if new_player.win_round = true then
      print_endline "You won the round"
    else print_endline "You lost the round";
    continue_playing (reset_player new_player) dealer_init )
  else player

(** [main ()] starts blackjack. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] welcome_string;
  (*Printing starting text- dealer and player hands printed*)
  ANSITerminal.print_string [ ANSITerminal.red ] start_round_string;
  print_endline dealer_card1_string;
  let player = start_round init_deck player_init dealer_init in
  if player.is_blackjack = true then print_endline "You got Blackjack"
  else ();
  if player.win_round = true then print_endline "You won the round"
  else print_endline "You lost the round";

  (* if player.win_round = false then print_endline "You Busted" else
     print_endline "You Won the Round"; *)
  let player_cont =
    continue_playing (reset_player player) dealer_init
  in

  (* if player_cont.win_round = false then print_endline "You Busted"
     else print_endline "You Won the Round";*)
  print_endline
    ( "Goodbye, you leave the game with "
    ^ string_of_int player_cont.chips
    ^ " chips." )

(* Execute the game engine. *)

let () = main ()
