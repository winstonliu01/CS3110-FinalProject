open Deck

(*type representing a player *)
type player = {
  hand : card list;
  hand_val : int;
  chips : int;
}

(*type representing a dealer *)
type dealer = {
  hand : card list;
  hand_val : int;
}

(*[point_add] is the value after the current card has been applied to
  the total*)
val point_add : int -> card -> int

(*[ace_checker] is the value of the ace depending on the scenario*)
val ace_checker : int -> int

(*[player_init] is the state of the player when the game begins*)
val player_init : player

(*[dealer_init] is the state of the dealer when the game begins*)
val dealer_init : dealer
