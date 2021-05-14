open Text
open Deck
open Command
open Player
open Round

(*ToDo: Correct betting amount *)

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

let bet_player (player : player) (bet_entered : int) =
  {
    hand = player.hand;
    hand_val = player.hand_val;
    chips = 100;
    bet = bet_entered;
    win_round = player.win_round;
    is_blackjack = player.is_blackjack;
  }

let rec enter_bet (player : player) =
  Text.place_bets ();
  let bet_placed = read_line () in
  match Command.check_bet bet_placed with
  | "empty" ->
      Text.empty_print ();
      enter_bet player
  | "invalid input" ->
      Text.invalid_print ();
      enter_bet player
  | _ ->
      let bet_entered = int_of_string bet_placed in
      if bet_entered >= player.chips then (
        Text.all_in ();
        let response = yes_no player in
        if response = "yes" then bet_player player player.chips
        else enter_bet player )
      else bet_player player bet_entered

(*ToDo: Incorporate betting for multiple players *)
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

let player_updated (player : player) chips_won =
  {
    hand = player.hand;
    hand_val = player.hand_val;
    chips = chips_won;
    bet = player.bet;
    win_round = player.win_round;
    is_blackjack = player.is_blackjack;
  }

let update_player (player : player) =
  if player.is_blackjack = true then
    let chips_won = player.bet * 2 in
    player_updated player chips_won
  else if player.win_round = 0 then
    let chips_won = player.bet / 2 in
    player_updated player chips_won
  else if player.win_round = 1 then
    let chips_won = player.bet / 2 in
    player_updated player chips_won
  else
    let chips_won = player.bet * -1 in
    player_updated player chips_won

(** [main ()] starts blackjack. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] welcome_string;
  ANSITerminal.print_string [ ANSITerminal.red ] start_round_string;

  let start_player = enter_bet player_init in
  let player = start_round init_deck start_player dealer_init in
  if player.is_blackjack = true then
    print_endline "You got Blackjack! \n"
  else ();
  if player.win_round = 1 then print_endline "You won the round! \n"
  else if player.win_round = 0 then
    print_endline "The round is a draw. \n"
  else if player.win_round = -2 then
    print_endline "Sorry, you busted! \n"
  else print_endline "You lost the round. \n";

  let finished_game = update_player player in
  let player_cont =
    continue_playing (reset_player finished_game) dealer_init
  in
  print_endline
    ( "\nGoodbye, you leave the game with "
    ^ string_of_int player_cont.chips
    ^ " chips. \n" )

(* Execute the game engine. *)
let () = main ()
