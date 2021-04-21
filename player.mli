open Deck

(*type representing a player *)
type player = {
  hand : card list;
  hand_val : int;
  chips : int;
  bet : int;
  win_round : bool;
}

(*type representing a dealer *)
type dealer = {
  hand : card list;
  hand_val : int;
}

(*[point_add_player] is the value after the current card has been
  applied to the total - and updates the player state*)
val point_add_player : int -> card -> player -> player

(*[point_add_dealer] is the value after the current card has been
  applied to the total - and updates the dealer state*)
val point_add_dealer : int -> card -> dealer -> dealer

(*[player_init] is the state of the player when the game begins*)
val player_init : player

(*[dealer_init] is the state of the dealer when the game begins*)
val dealer_init : dealer

(*[bust_checker_player] is if the player's hand value is over 21*)
val bust_checker_player : player -> bool

(*[bust_checker_dealer] is if the dealer's hand value is over 21*)
val bust_checker_player : player -> bool

(*[reset_player] is the state of the player after they bust or the round
  is over. Hand and hand value are cleared*)
val reset_player : player -> player
