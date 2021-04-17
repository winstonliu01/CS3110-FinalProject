open Deck

(*type representing a player *)
type player = {
  hand : card list;
  hand_val : int;
  chips : int;
}

type dealer = {
  hand : card list;
  hand_val : int;
}

val point_add : int -> card -> int

val ace_checker : int -> int

val player_init : player

val dealer_init : dealer
