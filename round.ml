open Text
open Deck
open Player

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

let rec parse_input deck total =
  let line = read_line () in
  match Command.check_string line with
  | "hit" ->
      if empty deck then (
        let new_deck = shuffle create in
        card new_deck;
        let new_deck_val = point_add total (draw new_deck) in
        print_string "> ";
        parse_input (remove new_deck) new_deck_val )
      else card deck;
      let new_val = point_add total (draw deck) in
      print_string "> ";
      parse_input (remove deck) new_val
  | "stay" ->
      print_endline ("Your total value is " ^ string_of_int total ^ ".")
  | _ ->
      print_string "\nInvalid input, please try again.\n\n";
      print_string "> ";
      parse_input deck total

let command deck total =
  print_endline h_or_s;
  print_string "> ";
  parse_input deck total

let start_round (deck : deck) (player : player) (dealer : dealer) =
  ANSITerminal.print_string [ ANSITerminal.red ] start_round_string;
  print_endline dealer_card1_string;
  print_hand (dealer.hand @ [ draw_deck deck ]);
  let deck1 = remove deck in
  print_endline "\nYour cards are...\n";
  print_hand (player.hand @ [ draw deck1 ]);
  let total1 = point_add 0 (draw deck1) in
  let deck2 = shuffle (remove deck1) in
  print_card (draw deck2);
  let total2 = point_add total1 (draw deck2) in
  command (shuffle (remove deck2)) total2

let init_deck = Deck.shuffle create
