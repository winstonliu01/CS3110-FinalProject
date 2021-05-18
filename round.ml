open Text
open Deck
open Player
open Cpu

let init_deck = Deck.shuffle create

let card deck =
  let c = draw deck in
  print_card c

let dd_draw (deck : deck) =
  card deck;
  draw deck

let player_start (deck : deck) (player : player) =
  let player_card = draw deck in
  let updated_player =
    point_add_player player.hand_val player_card player
  in
  let player =
    {
      hand = player.hand @ [ player_card ];
      hand_val = updated_player.hand_val;
      chips = player.chips;
      bet = player.bet;
      win_round = player.win_round;
      is_blackjack = player.is_blackjack;
      side_bet = player.side_bet;
      is_cpu = player.is_cpu;
    }
  in
  player

let dealer_start (deck : deck) (dealer : dealer) =
  let dealer_card = draw deck in
  let updated_dealer =
    point_add_dealer dealer.hand_val dealer_card dealer
  in
  let dealer =
    {
      hand = dealer.hand @ [ dealer_card ];
      hand_val = updated_dealer.hand_val;
    }
  in
  dealer

let player_total (player : player) =
  ANSITerminal.print_string [ ANSITerminal.red ]
    ("\nYour total value is " ^ string_of_int player.hand_val ^ ". ")

let dealer_total (dealer : dealer) =
  ANSITerminal.print_string [ ANSITerminal.red ]
    ("\nThe dealer's total is " ^ string_of_int dealer.hand_val ^ ". \n")

let rec dealer_cont (deck : deck) (dealer : dealer) =
  if dealer.hand_val < 17 then (
    let dealer_updated = dealer_start deck dealer in
    card deck;
    dealer_cont (remove deck) dealer_updated )
  else dealer

let blackjack_player_state (player : player) (win : int) =
  {
    hand = player.hand;
    hand_val = player.hand_val;
    chips = player.chips;
    bet = player.bet;
    win_round = win;
    is_blackjack = true;
    side_bet = player.side_bet;
    is_cpu = player.is_cpu;
  }

let regular_player_state (player : player) (win : int) =
  {
    hand = player.hand;
    hand_val = player.hand_val;
    chips = player.chips;
    bet = player.bet;
    win_round = win;
    is_blackjack = player.is_blackjack;
    side_bet = player.side_bet;
    is_cpu = player.is_cpu;
  }

let rec hit_player
    deck
    (player : player)
    (dealer : dealer)
    (cpu : player) =
  if empty deck then (
    let new_deck = shuffle create in
    card new_deck;
    let updated_player = player_start new_deck player in
    if bust_checker_player updated_player = true then
      regular_player_state player (-2)
    else parse_input new_deck updated_player dealer cpu )
  else (
    card deck;
    let updated_player = player_start deck player in
    if bust_checker_player updated_player = true then
      regular_player_state player (-2)
    else parse_input (remove deck) updated_player dealer cpu )

and stay_player deck (player : player) (dealer : dealer) (cpu : player)
    =
  player_total player;
  print_endline dealer_remaining_card;
  let dealer = dealer_cont deck dealer in
  if dealer.hand_val > 21 then print_endline "\nThe dealer busted!\n"
  else dealer_total dealer;

  if
    dealer.hand_val > player.hand_val
    && bust_checker_dealer dealer = false
  then
    let player' = regular_player_state player (-1) in
    player'
  else if dealer.hand_val = player.hand_val then
    regular_player_state player 0
  else if cpu.hand_val <= 21 && cpu.hand_val > player.hand_val then
    regular_player_state player (-3)
  else if cpu.hand_val = player.hand_val then
    regular_player_state player 2
  else regular_player_state player 1

and parse_input deck (player : player) (dealer : dealer) (cpu : player)
    =
  print_endline "Do you want to double down? \n";
  print_string "> ";
  let y_n = read_line () in
  match Command.check_yes_no y_n with
  | "yes" -> double_down deck player dealer cpu
  | "no" -> no_double_down deck player dealer cpu
  | "empty" ->
      Text.empty_print ();
      parse_input deck player dealer cpu
  | _ ->
      Text.invalid_print ();
      parse_input deck player dealer cpu

and no_double_down
    deck
    (player : player)
    (dealer : dealer)
    (cpu : player) =
  Text.h_or_s ();
  let line = read_line () in
  match Command.check_hit_stay line with
  | "hit" -> hit_player deck player dealer cpu
  | "stay" -> stay_player deck player dealer cpu
  | "empty" ->
      Text.empty_print ();
      parse_input deck player dealer cpu
  | _ ->
      Text.invalid_print ();
      parse_input deck player dealer cpu

and double_down deck player dealer cpu =
  let card = dd_draw deck in
  let deck' = remove deck in

  let player_hand' = point_add_player player.hand_val card player in
  let player' =
    {
      hand = player.hand @ [ card ];
      hand_val = player_hand'.hand_val;
      chips = player.chips;
      bet = player.bet * 2;
      win_round = player.win_round;
      is_blackjack = player.is_blackjack;
      side_bet = player.side_bet;
      is_cpu = player.is_cpu;
    }
  in
  if bust_checker_player player' = true then
    regular_player_state player' (-2)
  else stay_player deck' player' dealer cpu

let rec player_combo
    (player_card : card list)
    (combo : float)
    (ceiling : int) =
  match player_card with
  | [] -> combo
  | h :: t ->
      (*Fix the value for Ace*)
      if List.hd h.point <= ceiling then
        let combo' = combo -. 1. in
        player_combo t combo' ceiling
      else player_combo t combo ceiling

let start_game
    (deck : deck)
    (player : player)
    (dealer : dealer)
    (cpu : player)
    (lvl : string) =
  print_endline dealer_card1_string;
  print_card (draw deck);

  let dealer = dealer_start deck dealer in
  let updated_deck = remove deck in
  print_endline "\nYour first card is: \n";
  print_card (draw updated_deck);
  let player_1 = player_start updated_deck player in
  let updated_deck2 = remove updated_deck in
  print_endline "\nYour second card is: \n";
  print_card (draw updated_deck2);
  let player_2 = player_start updated_deck2 player_1 in

  let cpu_deck = remove updated_deck2 in
  (*This is where CPU Game changes*)
  let cpu_deck' = Cpu.run_cpu cpu_deck cpu dealer player_2 lvl in
  print_endline "\nNow it is your turn to play...\n";

  if black_jack_checker player_2 = true then (
    print_endline dealer_remaining_card;
    let cpu_deck'' = fst cpu_deck' in
    let dealer_blackjack = dealer_cont (remove cpu_deck'') dealer in
    if dealer_blackjack.hand_val = 21 then
      blackjack_player_state player_2 0
    else blackjack_player_state player_2 1 )
  else
    let cpu_deck'' = fst cpu_deck' in
    let updated_deck3 = remove cpu_deck'' in
    let cpu_updated = snd cpu_deck' in
    parse_input updated_deck3
      (player_2 : player)
      (dealer : dealer)
      (cpu_updated : player)

let player_sidebet (player : player) (side_bet : int) =
  {
    hand = player.hand;
    hand_val = player.hand_val;
    chips = player.chips;
    bet = player.bet;
    win_round = player.win_round;
    is_blackjack = player.is_blackjack;
    side_bet;
    is_cpu = player.is_cpu;
  }

let rec side_bet (deck : deck) (player : player) (dealer : dealer) =
  print_endline "Your side bet options are: \n";
  print_endline "1. Your hand value is an odd number (5 chips) \n";
  print_endline "2. Your hand value is a prime number (10 chips)\n";
  print_endline "3. Your hand value is only hearts (50 chips) \n";
  print_endline "Enter 1,2,3 as an input (one selection allowed) \n";
  print_string "> ";
  let s_b = read_line () in
  match Command.check_side_bet s_b with
  | "1" -> player_sidebet player 1
  | "2" -> player_sidebet player 2
  | "3" -> player_sidebet player 3
  | "empty" ->
      Text.empty_print ();
      side_bet deck player dealer
  | _ ->
      Text.invalid_print ();
      side_bet deck player dealer

let rec start_round
    (deck : deck)
    (player : player)
    (dealer : dealer)
    (cpu : player)
    (lvl : string) =
  print_endline "Do you want to do a side bet? \n";
  print_string "> ";
  let y_n = read_line () in
  match Command.check_yes_no y_n with
  | "yes" ->
      let player' = side_bet deck player dealer in
      start_game deck player' dealer cpu lvl
  | "no" -> start_game deck player dealer cpu lvl
  | "empty" ->
      Text.empty_print ();
      start_round deck player dealer cpu lvl
  | _ ->
      Text.invalid_print ();
      start_round deck player dealer cpu lvl
