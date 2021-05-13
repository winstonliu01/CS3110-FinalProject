open Text
open Deck
open Player

let init_deck = Deck.shuffle create

let card deck =
  let c = draw deck in
  print_card c

let draw_deck deck =
  card deck;
  draw (remove deck)

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
  print_endline
    ("\nYour total value is " ^ string_of_int player.hand_val ^ ". ")

let dealer_total (dealer : dealer) =
  "\nThe dealer's total is " ^ string_of_int dealer.hand_val ^ ". \n"

let rec dealer_cont (deck : deck) (dealer : dealer) =
  if dealer.hand_val < 17 then (
    let dealer_updated = dealer_start deck dealer in
    card deck;
    dealer_cont (remove deck) dealer_updated )
  else dealer

let bust_player (player : player) =
  {
    hand = player.hand;
    hand_val = player.hand_val;
    chips = player.chips;
    bet = player.bet;
    win_round = -2;
    is_blackjack = player.is_blackjack;
  }

let dealer_player_bj_tie (player : player) =
  {
    hand = player.hand;
    hand_val = player.hand_val;
    chips = player.chips;
    bet = player.bet;
    win_round = 0;
    is_blackjack = true;
  }

let player_bj_win (player : player) =
  {
    hand = player.hand;
    hand_val = player.hand_val;
    chips = player.chips;
    bet = player.bet;
    win_round = 1;
    is_blackjack = true;
  }

let player_lost (player : player) =
  {
    hand = player.hand;
    hand_val = player.hand_val;
    chips = player.chips;
    bet = player.bet;
    win_round = -1;
    is_blackjack = player.is_blackjack;
  }

let player_dealer_tie (player : player) =
  {
    hand = player.hand;
    hand_val = player.hand_val;
    chips = player.chips;
    bet = player.bet;
    win_round = 0;
    is_blackjack = player.is_blackjack;
  }

let player_win (player : player) =
  {
    hand = player.hand;
    hand_val = player.hand_val;
    chips = player.chips;
    bet = player.bet;
    win_round = 1;
    is_blackjack = player.is_blackjack;
  }

let rec hit_player deck (player : player) (dealer : dealer) =
  if empty deck then (
    let new_deck = shuffle create in
    card new_deck;
    let updated_player = player_start new_deck player in
    if bust_checker_player updated_player = true then bust_player player
    else parse_input new_deck updated_player dealer )
  else (
    card deck;
    let updated_player = player_start deck player in
    if bust_checker_player updated_player = true then bust_player player
    else parse_input (remove deck) updated_player dealer )

and stay_player deck (player : player) (dealer : dealer) =
  player_total player;
  print_endline dealer_remaining_card;
  let dealer = dealer_cont deck dealer in
  if dealer.hand_val > 21 then print_endline "\nThe dealer busted!\n"
  else print_endline (dealer_total dealer);

  if
    dealer.hand_val > player.hand_val
    && bust_checker_dealer dealer = false
  then
    let player' = player_lost player in
    player'
  else if dealer.hand_val = player.hand_val then
    player_dealer_tie player
  else player_win player

and parse_input deck (player : player) (dealer : dealer) =
  (*Not printing anything*)
  h_or_s;
  let line = read_line () in
  match Command.check_hit_stay line with
  | "hit" -> hit_player deck player dealer
  | "stay" -> stay_player deck player dealer
  (*Not printing empty or invalid input*)
  | "empty" ->
      empty_print;
      parse_input deck player dealer
  | _ ->
      invalid_print;
      parse_input deck player dealer

let start_round (deck : deck) (player : player) (dealer : dealer) =
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
  if black_jack_checker player_2 = true then (
    print_endline dealer_remaining_card;
    let dealer_blackjack = dealer_cont (remove updated_deck2) dealer in

    if dealer_blackjack.hand_val = 21 then dealer_player_bj_tie player_2
    else player_bj_win player_2 )
  else
    let updated_deck3 = remove updated_deck2 in
    parse_input updated_deck3 (player_2 : player) (dealer : dealer)
