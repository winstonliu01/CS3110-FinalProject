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

let rec parse_input deck (player : player) (dealer : dealer) =
  let line = read_line () in
  match Command.check_hit_stay line with
  | "hit" ->
      (*Empty Deck: Create a newly shuffled deck Draw a card and extract
        the point value Then parse for a hit or stay command *)
      if empty deck then (
        let new_deck = shuffle create in
        let card_drawn = draw_deck new_deck in
        let updated_player =
          point_add_player player.hand_val card_drawn player
        in
        print_string "> ";
        parse_input new_deck updated_player dealer )
      else
        (*Draw a card and extract point value*)
        let card_drawn = draw_deck deck in
        let updated_player =
          point_add_player player.hand_val card_drawn player
        in
        print_string "> ";
        parse_input (remove deck) updated_player dealer
  | "stay" ->
      print_endline
        ("Your total value is " ^ string_of_int player.hand_val ^ ".");
      player
  | _ ->
      print_endline "\nInvalid input, please try again.";
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

let player_start (deck : deck) (player : player) =
  let dealer_card = draw deck in
  let updated_dealer =
    point_add_player player.hand_val dealer_card player
  in
  let player =
    {
      hand = player.hand @ [ dealer_card ];
      hand_val = updated_dealer.hand_val;
      chips = player.chips;
      bet = player.bet;
      win_round = player.win_round;
    }
  in
  player

(*ToDo - Fix the hand value. Point add is not quite correct*)

let start_round (deck : deck) (player : player) (dealer : dealer) =
  print_card (draw deck);
  let dealer = dealer_start deck dealer in
  let updated_deck = remove deck in
  print_endline "Your first card is: ";
  print_card (draw updated_deck);
  let player = player_start updated_deck player in
  let updated_deck2 = remove updated_deck in
  print_endline "Your second card is: ";
  print_card (draw updated_deck2);
  print_endline h_or_s;
  print_string "> ";
  let updated_deck3 = remove updated_deck2 in
  let player = player_start updated_deck3 player in
  parse_input updated_deck3 (player : player) (dealer : dealer)

(* (*Adding drawn card to dealer.hand*) print_hand (dealer.hand @ [ draw
   deck ]); let deck1 = remove deck in let deck2 = shuffle (remove
   deck1) in print_endline "\nYour cards\n are...\n"; print_hand
   (player.hand @ [ draw deck1 ]); print_hand (player.hand @ [ draw
   deck2 ]); (*Update field and do something else?*) print_endline
   h_or_s; print_string "> "*)

(*Make play - circular dependency*)
(* parse_input (shuffle (remove deck2)) player.hand_val player dealer*)

(* player.hand_val = point_add 0 (draw deck1) + point_add 0 (draw
   deck2); parse_input (shuffle (remove deck2)) player.hand_val player
   dealer *)

(* let rec print_hand (deck : card list) = match deck with | [] -> () |
   h :: t -> print_card h; print_hand t

   (*ToDo - Fix hand_val*) let start_round (deck : deck) (player :
   player) (dealer : dealer) = (*Adding drawn card to dealer.hand*)
   print_hand (dealer.hand @ [ draw deck ]); let deck1 = remove deck in
   let deck2 = shuffle (remove deck1) in print_endline "\nYour cards
   are...\n"; print_hand (player.hand @ [ draw deck1 ]); print_hand
   (player.hand @ [ draw deck2 ]); (*Update field and do something
   else?*) print_endline h_or_s; print_string "> "

   (*Make play - circular dependency*) (* parse_input (shuffle (remove
   deck2)) player.hand_val player dealer*)

   (* player.hand_val = point_add 0 (draw deck1) + point_add 0 (draw
   deck2); parse_input (shuffle (remove deck2)) player.hand_val player
   dealer *) *)
