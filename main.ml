open Text
open Deck
open Command
open Player
open Round

let start_new_round deck player dealer =
  ANSITerminal.print_string [ ANSITerminal.red ] new_round_string;
  start_round deck player dealer

let rec yes_no (player : player) =
  let y_n = read_line () in
  match Command.check_yes_no y_n with
  | "yes" -> "yes"
  | "no" -> "no"
  | "empty" ->
      Text.empty_print ();
      yes_no player
  | _ ->
      Text.invalid_print ();
      yes_no player

let rec continue_playing (player : player) (dealer : dealer) =
  print_endline "Would you like to play another round? \n";
  print_string "> ";
  let response = yes_no player in
  if response = "yes" then (
    let new_player =
      start_new_round (shuffle init_deck) player dealer_init
    in
    if new_player.is_blackjack = true then
      print_endline "You got Blackjack! \n"
    else ();
    if new_player.win_round = 1 then
      print_endline "You won the round! \n"
    else if new_player.win_round = 0 then
      print_endline "The round is a draw. \n"
    else if new_player.win_round = -2 then
      print_endline "Sorry, you busted! \n"
    else print_endline "You lost the round. \n";
    continue_playing (reset_player new_player) dealer_init )
  else player

(** [main ()] starts blackjack. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] welcome_string;
  ANSITerminal.print_string [ ANSITerminal.red ] start_round_string;

  let player = start_round init_deck player_init dealer_init in
  if player.is_blackjack = true then
    print_endline "You got Blackjack! \n"
  else ();
  if player.win_round = 1 then print_endline "You won the round! \n"
  else if player.win_round = 0 then
    print_endline "The round is a draw. \n"
  else if player.win_round = -2 then
    print_endline "Sorry, you busted! \n"
  else print_endline "You lost the round. \n";

  let player_cont =
    continue_playing (reset_player player) dealer_init
  in
  print_endline
    ( "\nGoodbye, you leave the game with "
    ^ string_of_int player_cont.chips
    ^ " chips. \n" )

(* Execute the game engine. *)

let () = main ()
