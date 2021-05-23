open Text
open Deck
open Command
open Player
open Round

(*Code was referenced from
  http://www.cs.cornell.edu/courses/cs3110/2010fa/lectures/lec02.html*)

(** [isPrime n] checks if a hand contains a prime number *)
let isPrime (n : int) : bool =
  let rec noDivisors (m : int) : bool =
    m * m > n || (n mod m != 0 && noDivisors (m + 1))
  in
  n >= 2 && noDivisors 2

(** [heart_suite hand] checks if side bet 3 has been met *)
let rec heart_suite (hand : card list) =
  match hand with
  | [] -> true
  | h :: t -> if h.suite <> "♥" then false else heart_suite t

(** [check_side_bet player] asks the user for a side bet selection *)
let check_side_bet (player : player) =
  match player.side_bet with
  | 1 -> if player.hand_val mod 2 <> 0 then 5 else -5
  | 2 -> if isPrime player.hand_val then 10 else -10
  | 3 -> if heart_suite player.hand then 50 else -50
  | _ -> 0

(** [yes_no player] asks the user for a yes/no selection *)
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

(** [level player] asks the user what CPU level they want to play *)
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

(** [player_updated player] updates the player state *)
let player_updated (player : player) chips_won (bet_entered : int) =
  {
    hand = player.hand;
    hand_val = player.hand_val;
    chips = player.chips + chips_won;
    bet = player.bet + bet_entered;
    win_round = player.win_round;
    is_blackjack = player.is_blackjack;
    side_bet = player.side_bet;
    is_cpu = player.is_cpu;
  }

(** [cpu_bet cpu] bets for the CPU *)
let cpu_bet (cpu : player) =
  let _ = Random.self_init () in
  let num1 = Random.int 9 + 1 in
  let num2 = Random.int 4 + 1 in
  if cpu.chips <= 50 then player_updated cpu 0 (cpu.chips / num1)
  else
    let bet = Stdlib.min (cpu.chips / num1 * num2) (cpu.chips / num2) in
    player_updated cpu 0 bet

(** [update_player player] updates the player's chips based on their bet *)
let update_player (player : player) =
  let side_bet_chips = check_side_bet player in
  if player.is_blackjack = true then
    let chips_won = player.bet * 2 in
    player_updated player (chips_won + side_bet_chips) 0
  else if player.win_round = 0 then
    let chips_won = player.bet / 2 in
    player_updated player (chips_won + side_bet_chips) 0
  else if player.win_round = 1 then
    let chips_won = player.bet in
    player_updated player (chips_won + side_bet_chips) 0
  else
    let chips_won = player.bet * -1 in
    player_updated player (chips_won + side_bet_chips) 0

(** [blackjack_print player] tells the player that they got blackjack *)
let blackjack_print (player : player) =
  if player.is_blackjack = true then
    print_endline "\nYou got Blackjack! \n"
  else ()

(** [enter_bet player] asks the player for a bet at the start of the
    round. *)
let rec enter_bet (player : player) =
  print_endline
    ("\nYou now have " ^ string_of_int player.chips ^ " chips.");
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
        if response = "yes" then player_updated player 0 player.chips
        else enter_bet player )
      else player_updated player 0 bet_entered

(**[print_results p1 user] Displays to console the result of the round
   for both CPU and player*)
let print_results (p1 : player) (user : string) =
  if p1.win_round = 1 then print_endline (user ^ ": " ^ Text.win)
  else if p1.win_round = 0 then print_endline (user ^ ": " ^ Text.draw)
  else if p1.win_round = -2 then print_endline (user ^ ": " ^ Text.bust)
  else print_endline (user ^ ": " ^ Text.loss_dealer)

(**[cpu_bet_print] prints how many chips the CPU has and bet*)
let cpu_bet_print (cpu_bet : player) =
  print_endline
    ("\n The CPU has " ^ string_of_int cpu_bet.chips ^ " chips.");
  print_endline
    ( "\nThe CPU decides to bet "
    ^ string_of_int cpu_bet.bet
    ^ " chips.\n" )

(** [continue_playing player dealer cpu] starts an additional round of
    blackjack passing in states from the prior game. *)
let rec continue_playing
    (player : player)
    (dealer : dealer)
    (cpu : player) =
  print_endline more_round;
  print_string "> ";
  let response = yes_no player in
  if response = "yes" && player.chips > 0 && cpu.chips > 0 then (
    ANSITerminal.print_string [ ANSITerminal.red ] new_round_string;
    let bet_player = enter_bet player in
    Text.level ();
    let cpu_lvl = level bet_player in
    let cpu_bet = cpu_bet cpu in
    cpu_bet_print cpu_bet;
    let new_player =
      Round.start_round (shuffle init_deck) bet_player dealer_init
        cpu_bet cpu_lvl
    in
    let p1 = fst new_player in
    let p2 = snd new_player in
    blackjack_print p1;

    print_results p1 "Player";
    print_results p2 "CPU";

    let finished_multiple_game_p1 = update_player p1 in
    let finished_multiple_game_p2 = update_player p2 in
    continue_playing
      (reset_player finished_multiple_game_p1)
      dealer_init
      (reset_cpu finished_multiple_game_p2) )
  else (player, cpu)

(** [main ()] starts blackjack. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ] welcome_string;
  ANSITerminal.print_string [ ANSITerminal.red ] start_round_string;

  let start_player = enter_bet player_init in
  Text.level ();
  let cpu_lvl = level start_player in
  let cpu_bet = cpu_bet cpu_init in
  cpu_bet_print cpu_bet;
  let player =
    Round.start_round init_deck start_player dealer_init cpu_bet cpu_lvl
  in
  let p1 = fst player in
  let p2 = snd player in
  blackjack_print p1;

  print_results p1 "Player";
  print_results p2 "CPU";

  let finished_game_p1 = update_player p1 in
  let finished_game_p2 = update_player p2 in
  let player_cont =
    continue_playing
      (reset_player finished_game_p1)
      dealer_init
      (reset_cpu finished_game_p2)
  in

  let p1 = fst player_cont in
  let p2 = snd player_cont in

  if p1.chips <= 0 then print_endline Text.bankrupt
  else if p2.chips <= 0 then
    print_endline "\nCongrats, you defeated the CPU!\n"
  else
    print_endline
      ( "\nGoodbye, you leave the game with " ^ string_of_int p1.chips
      ^ " chips. \n" )

(* Execute the game engine. *)
let () = main ()
