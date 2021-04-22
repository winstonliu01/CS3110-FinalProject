open Text
open Deck
open Player

(*ToDo - parse_input Add a case where the message tells you it is empty *)

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
    }
  in
  player

let rec parse_input deck (player : player) (dealer : dealer) =
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
        print_string "> ";
        parse_input new_deck updated_player dealer
        (* if bust_checker_player player = false then player else
           parse_input new_deck updated_player dealer*) )
      else (
        (*Draw a card and extract point value let card_drawn = draw_deck
          deck in*)
        card deck;
        let updated_player = player_start deck player in
        print_string "> ";
        parse_input (remove deck) updated_player dealer
        (* if bust_checker_player player = false then player else
           parse_input (remove deck) updated_player dealer *) )
  | "stay" ->
      print_endline
        ( "\nYour total value is "
        ^ string_of_int player.hand_val
        ^ ". \n" );
      (* if bust_checker_player player = false then player else { hand =
         player.hand; hand_val = player.hand_val; chips = player.chips;
         bet = player.bet; win_round = true; } *)
      {
        hand = player.hand;
        hand_val = player.hand_val;
        chips = player.chips;
        bet = player.bet;
        win_round = true;
      }
  | "empty" ->
      print_endline "\nEmpty input, please try again. \n";
      print_string "> ";
      parse_input deck player dealer
  | _ ->
      print_endline "\nInvalid input, please try again. \n";
      print_string "> ";
      parse_input deck player dealer

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

let start_round (deck : deck) (player : player) (dealer : dealer) =
  print_card (draw deck);
  let dealer = dealer_start deck dealer in
  let updated_deck = remove deck in
  print_endline "Your first card is: ";
  print_card (draw updated_deck);
  let player_1 = player_start updated_deck player in
  let updated_deck2 = remove updated_deck in
  print_endline "Your second card is: ";
  print_card (draw updated_deck2);
  let player_2 = player_start updated_deck2 player_1 in
  print_endline h_or_s;
  print_string "> ";
  let updated_deck3 = remove updated_deck2 in
  parse_input updated_deck3 (player_2 : player) (dealer : dealer)

(*ToDo - Implement Blackjack check at line 108*)
