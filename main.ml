open Text
open Deck
open Command
open Player
open Round

let start_new_round deck player dealer =
  start_round deck player dealer cpu_init

(*Code was referenced from
  http://www.cs.cornell.edu/courses/cs3110/2010fa/lectures/lec02.html*)
let isPrime (n : int) : bool =
  let rec noDivisors (m : int) : bool =
    m * m > n || (n mod m != 0 && noDivisors (m + 1))
  in
  n >= 2 && noDivisors 2

let rec heart_suite (hand : card list) =
  match hand with
  | [] -> true
  | h :: t -> if h.suite <> "♥" then false else heart_suite t

let check_side_bet (player : player) =
  match player.side_bet with
  | 1 -> if player.hand_val mod 2 <> 0 then 5 else -5
  | 2 -> if isPrime player.hand_val then 10 else -10
  | 3 -> if heart_suite player.hand then 50 else -50
  | _ -> 0

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

let rec level (player : player) =
  let lvl = read_line () in
  match Command.check_level lvl with
  | "1" -> "1"
  | "2" -> "2"
  | "empty" ->
      Text.empty_print ();
      level player
  | _ ->
      Text.invalid_print ();
      level player

let bet_player (player : player) (bet_entered : int) =
  {
    hand = player.hand;
    hand_val = player.hand_val;
    chips = player.chips;
    bet = bet_entered;
    win_round = player.win_round;
    is_blackjack = player.is_blackjack;
    side_bet = player.side_bet;
    is_cpu = player.is_cpu;
  }

let player_updated (player : player) chips_won =
  {
    hand = player.hand;
    hand_val = player.hand_val;
    chips = player.chips + chips_won;
    bet = player.bet;
    win_round = player.win_round;
    is_blackjack = player.is_blackjack;
    side_bet = player.side_bet;
    is_cpu = player.is_cpu;
  }

let update_player (player : player) =
  let side_bet_chips = check_side_bet player in
  if player.is_blackjack = true then
    let chips_won = player.bet * 2 in
    player_updated player (chips_won + side_bet_chips)
  else if player.win_round = 0 then
    let chips_won = player.bet / 2 in
    player_updated player (chips_won + side_bet_chips)
  else if player.win_round = 1 then
    let chips_won = player.bet in
    player_updated player (chips_won + side_bet_chips)
  else
    let chips_won = player.bet * -1 in
    player_updated player (chips_won + side_bet_chips)

let blackjack_print (player : player) =
  if player.is_blackjack = true then
    print_endline "You got Blackjack! \n"
  else ()

let rec enter_bet (player : player) =
  print_endline
    ("You now have " ^ string_of_int player.chips ^ " chips.");
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

(*Fix print_results - always entering last branch*)
let print_results (p1 : player) =
  if p1.win_round = 1 then print_endline Text.win
  else if p1.win_round = 0 then print_endline Text.draw
  else if p1.win_round = -2 then print_endline Text.bust
  else print_endline Text.loss_dealer

let rec continue_playing
    (player : player)
    (dealer : dealer)
    (cpu : player) =
  print_endline more_round;
  print_string "> ";
  let response = yes_no player in
  if response = "yes" && player.chips > 0 then (
    ANSITerminal.print_string [ ANSITerminal.red ] new_round_string;
    let bet_player = enter_bet player in
    Text.level ();
    let cpu_lvl = level bet_player in
    let new_player =
      start_new_round (shuffle init_deck) bet_player dealer_init cpu_lvl
    in
    let p1 = fst new_player in
    let p2 = snd new_player in
    blackjack_print p1;
    (*Delete when done*)
    print_string (string_of_int p1.win_round);
    print_string (string_of_int p2.win_round);
    print_results p1;
    let finished_multiple_game = update_player p1 in
    continue_playing
      (reset_player finished_multiple_game)
      dealer_init cpu )
  else player

(** [main ()] starts blackjack. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] welcome_string;
  ANSITerminal.print_string [ ANSITerminal.red ] start_round_string;

  let start_player = enter_bet player_init in
  Text.level ();
  let cpu_lvl = level start_player in
  let player =
    start_round init_deck start_player dealer_init cpu_init cpu_lvl
  in
  let p1 = fst player in
  let p2 = snd player in
  blackjack_print p1;
  (*Delete when done*)
  print_string (string_of_int p1.win_round);
  print_string (string_of_int p2.win_round);

  print_results p1;

  let finished_game = update_player p1 in
  let player_cont =
    continue_playing
      (reset_player finished_game)
      dealer_init (reset_player p2)
  in

  if player_cont.chips <= 0 then print_endline Text.bankrupt
  else
    print_endline
      ( "\nGoodbye, you leave the game with "
      ^ string_of_int player_cont.chips
      ^ " chips. \n" )

(* Execute the game engine. *)
let () = main ()
