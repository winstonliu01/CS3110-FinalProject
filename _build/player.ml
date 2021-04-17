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

let player_init = { hand = []; hand_val = 0; chips = 500 }

let dealer_init = { hand = []; hand_val = 0 }

let ace_checker total =
  if total + 11 >= 22 then
    let total = 1 in
    total
  else
    let total = 11 in
    total

let point_add total card =
  match card.rank with
  | "A" -> total + ace_checker total
  | _ -> total + List.hd card.point
