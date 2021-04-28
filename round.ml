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

let rec dealer_cont (deck : deck) (dealer : dealer) =
  if dealer.hand_val < 17 then (
    let dealer_updated = dealer_start deck dealer in
    card deck;
    dealer_cont (remove deck) dealer_updated )
  else dealer

let rec parse_input deck (player : player) (dealer : dealer) =
  print_endline ("\n" ^ h_or_s);
  print_string "> ";
  let line = read_line () in
  match Command.check_hit_stay line with
  | "hit" ->
      if
        (*Empty Deck: Create a newly shuffled deck Draw a card and
          extract the point value Then parse for a hit or stay command *)
        empty deck
      then (
        let new_deck = shuffle create in
        card new_deck;
        let updated_player = player_start new_deck player in
        if bust_checker_player updated_player = true then
          {
            hand = player.hand;
            hand_val = player.hand_val;
            chips = player.chips;
            bet = player.bet;
            win_round = -2;
            is_blackjack = player.is_blackjack;
          }
        else parse_input new_deck updated_player dealer )
      else (
        (*Draw a card and extract point value let card_drawn = draw_deck
          deck in*)
        card deck;
        let updated_player = player_start deck player in
        if bust_checker_player updated_player = true then
          {
            hand = player.hand;
            hand_val = player.hand_val;
            chips = player.chips;
            bet = player.bet;
            win_round = -2;
            is_blackjack = player.is_blackjack;
          }
        else parse_input (remove deck) updated_player dealer )
  | "stay" ->
      print_endline
        ("\nYour total value is " ^ string_of_int player.hand_val ^ ". ");
      print_endline
        "\nThe dealer's hidden card and remaining cards are:\n";
      let dealer = dealer_cont deck dealer in
      if dealer.hand_val > 21 then
        print_endline "\nThe dealer busted!\n"
      else
        print_endline
          ( "\nThe dealer's total is "
          ^ string_of_int dealer.hand_val
          ^ ". \n" );

      (*Check if there is a non-brute force way if dealer.hand_val >
        player.hand_val then player.win_round = false else
        player.win_round = true*)
      if
        dealer.hand_val > player.hand_val
        && bust_checker_dealer dealer = false
      then
        let player =
          {
            hand = player.hand;
            hand_val = player.hand_val;
            chips = player.chips;
            bet = player.bet;
            win_round = -1;
            is_blackjack = player.is_blackjack;
          }
        in
        player
      else if dealer.hand_val = player.hand_val then
        {
          hand = player.hand;
          hand_val = player.hand_val;
          chips = player.chips;
          bet = player.bet;
          win_round = 0;
          is_blackjack = player.is_blackjack;
        }
      else
        {
          hand = player.hand;
          hand_val = player.hand_val;
          chips = player.chips;
          bet = player.bet;
          win_round = 1;
          is_blackjack = player.is_blackjack;
        }
  | "empty" ->
      print_endline "\nEmpty input, please try again.";
      parse_input deck player dealer
  | _ ->
      print_endline "\nInvalid input, please try again.\n";
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
  if black_jack_checker player_2 = true then
    let dealer_blackjack = dealer_cont (remove updated_deck2) dealer in
    if dealer_blackjack.hand_val = 21 then
      {
        hand = player_2.hand;
        hand_val = player_2.hand_val;
        chips = player_2.chips;
        bet = player_2.bet;
        win_round = 0;
        is_blackjack = true;
      }
    else
      {
        hand = player_2.hand;
        hand_val = player_2.hand_val;
        chips = player_2.chips;
        bet = player_2.bet;
        win_round = 1;
        is_blackjack = true;
      }
  else
    let updated_deck3 = remove updated_deck2 in
    parse_input updated_deck3 (player_2 : player) (dealer : dealer)
