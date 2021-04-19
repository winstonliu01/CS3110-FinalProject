open Deck

type player = {
  hand : card list;
  hand_val : int;
  chips : int;
}

type dealer = {
  hand : card list;
  hand_val : int;
}

let player_init = { hand = []; hand_val = 0; chips = 100 }

let dealer_init = { hand = []; hand_val = 0 }

let reset_player player =
  { hand = []; hand_val = 0; chips = player.chips }

let reset_dealer (dealer : dealer) = { hand = []; hand_val = 0 }

(* Cases: 1. Bust with 11 but not 1 \n 2. Bust with both 11 or 1 \n 3.
   No bust with either 11 or 1 *)

let rec ace_counter_helper hand acc =
  match hand with
  | [] -> acc
  | h :: t ->
      if h.rank = "A" then ace_counter_helper t acc + 1
      else ace_counter_helper t acc

let ace_count (player : player) = ace_counter_helper player.hand 0

(* n is the amount of Aces: n * 1 or 11 + (n-1) *)

let one_ace total =
  if total + 11 >= 22 && total + 1 <= 21 then
    let total = 1 in
    total
  else if total + 11 <= 21 && total + 1 <= 21 then
    (*User input to decide*)
    let total = 11 in
    total
  else 100

let multi_ace num total =
  if 11 + (num - 1) > 21 then total + num else total + 11 + (num - 1)

let ace_checker total (player : player) =
  let num_aces = ace_count player in
  if num_aces = 1 then one_ace total else multi_ace num_aces total

let point_add total card (player : player) =
  match card.rank with
  | "A" -> total + ace_checker total player
  | _ -> total + List.hd card.point
