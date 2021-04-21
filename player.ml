open Deck
open Command

type player = {
  hand : card list;
  hand_val : int;
  chips : int;
  bet : int;
  win_round : bool;
}

type dealer = {
  hand : card list;
  hand_val : int;
}

let player_init =
  { hand = []; hand_val = 0; chips = 100; bet = 0; win_round = false }

let dealer_init = { hand = []; hand_val = 0 }

let reset_player player =
  {
    hand = [];
    hand_val = 0;
    chips = player.chips;
    bet = 0;
    win_round = false;
  }

let rec ace_value temp =
  let line = read_line () in
  match Command.check_1_11 line with
  | "1" -> 1
  | "11" -> 11
  | _ ->
      print_string "\nInvalid input, please try again.\n\n";
      print_string "> ";
      ace_value 0

(*If Ace is 11 and bust, we set it to one which could bust as well. But
  we take care of that later. Otherwise if it doesn't we let the user
  decide.*)
let ace_checker total = if total + 11 >= 21 then 1 else ace_value 0

let bust_checker_player (player : player) = player.hand_val > 21

let bust_checker_dealer (dealer : dealer) = dealer.hand_val > 21

let point_add_player total card (player : player) =
  match card.rank with
  | "A" ->
      let updated_total = total + ace_checker total in
      {
        hand = player.hand;
        hand_val = updated_total;
        chips = player.chips;
        bet = player.bet;
        win_round = player.win_round;
      }
  | _ ->
      let updated_total = total + List.hd card.point in
      {
        hand = player.hand;
        hand_val = updated_total;
        chips = player.chips;
        bet = player.bet;
        win_round = player.win_round;
      }

let point_add_dealer total card (dealer : dealer) =
  match card.rank with
  | "A" ->
      let updated_total = total + ace_checker total in
      { hand = dealer.hand; hand_val = updated_total }
  | _ ->
      let updated_total = total + List.hd card.point in
      { hand = dealer.hand; hand_val = updated_total }
