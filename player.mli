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
val point_add : int -> card -> player -> int

(*[ace_checker] is the value of the ace depending on the scenario*)
val ace_checker : int -> player -> int

val multi_ace : int -> int -> int

val one_ace : int -> int

val ace_count : player -> int

(*[player_init] is the state of the player when the game begins*)
val player_init : player

(*[dealer_init] is the state of the dealer when the game begins*)
val dealer_init : dealer

(*[reset_player] is the state of the player after they bust or the round
  is over. Hand and hand value are cleared*)
val reset_player : player -> player

(*[reset_dealer] is the state of the dealer after they bust the round is
  over. Hand and hand value are cleared*)
val reset_dealer : dealer -> dealer
