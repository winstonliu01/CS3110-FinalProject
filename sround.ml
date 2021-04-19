open Text
open Deck
open Player
open Round

let rec print_hand (deck : card list) =
  match deck with
  | [] -> ()
  | h :: t ->
      print_card h;
      print_hand t

(*ToDo - Fix hand_val*)
let start_round (deck : deck) (player : player) (dealer : dealer) =
  (*Adding drawn card to dealer.hand*)
  print_hand (dealer.hand @ [ draw deck ]);
  let deck1 = remove deck in
  let deck2 = shuffle (remove deck1) in
  print_endline "\nYour cards are...\n";
  print_hand (player.hand @ [ draw deck1 ]);
  print_hand (player.hand @ [ draw deck2 ]);
  (*Update field and do something else?*)
  print_endline h_or_s;
  print_string "> "

(*Make play - circular dependency*)
(* parse_input (shuffle (remove deck2)) player.hand_val player dealer*)

(* player.hand_val = point_add 0 (draw deck1) + point_add 0 (draw
   deck2); parse_input (shuffle (remove deck2)) player.hand_val player
   dealer *)
