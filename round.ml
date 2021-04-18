open Text
open Deck
open Player

let init_deck = Deck.shuffle create

let card deck =
  let c = draw deck in
  print_card c

let draw_deck deck =
  card deck;
  draw (remove deck)

let rec print_hand (deck : card list) =
  match deck with
  | [] -> ()
  | h :: t ->
      print_card h;
      print_hand t

let bust_p (player : player) =
  if player.hand_val > 21 then true else false

let bust_d (dealer : dealer) =
  if dealer.hand_val > 21 then true else false

let rec parse_input deck total (player : player) (dealer : dealer) =
  let line = read_line () in
  match Command.check_string line with
  | "hit" ->
      (*Empty Deck: Create a newly shuffled deck Draw a card and extract
        the point value Then parse for a hit or stay command *)
      if empty deck then (
        let new_deck = shuffle create in
        let card_drawn = draw_deck new_deck in
        let new_deck_val = point_add total card_drawn in
        print_string "> ";
        parse_input new_deck new_deck_val player dealer )
      else
        (*Draw a card and extract point value*)
        let card_drawn = draw_deck deck in
        let new_val = point_add total card_drawn in
        print_string "> ";
        parse_input (remove deck) new_val player dealer
  | "stay" ->
      print_endline ("Your total value is " ^ string_of_int total ^ ".");
      print_string "The round is over, do you want to continue?";
      print_string "> "
      (* let y_n = read_line () in match Command.check_yn y_n with |
         "yes" -> print_endline "Start a new round." | "no" ->
         print_endline "Goodbye, thanks for playing." | _ ->
         print_string "\nInvalid input, please try again.\n\n";
         print_string "> "; (*Worry about this later*) parse_input deck
         total player dealer )*)
  | _ ->
      print_string "\nInvalid input, please try again.\n\n";
      print_string "> ";
      parse_input deck total player dealer

let command deck total (player : player) (dealer : dealer) =
  print_endline h_or_s;
  print_string "> ";
  parse_input deck total player dealer

let start_round (deck : deck) (player : player) (dealer : dealer) =
  (*Printing starting text- dealer and player hands printed*)
  ANSITerminal.print_string [ ANSITerminal.red ] start_round_string;
  print_endline dealer_card1_string;
  (*Adding drawn card to dealer.hand*)
  print_hand (dealer.hand @ [ draw deck ]);
  let deck1 = remove deck in
  print_endline "\nYour cards are...\n";
  print_hand (player.hand @ [ draw deck1 ]);
  let total1 = point_add 0 (draw deck1) in
  let deck2 = shuffle (remove deck1) in
  print_card (draw deck2);
  let total2 = point_add total1 (draw deck2) in
  command (shuffle (remove deck2)) total2 player dealer
