open Text
open Deck
open Player
open Sround

let init_deck = Deck.shuffle create

let card deck =
  let c = draw deck in
  print_card c

let draw_deck deck =
  card deck;
  draw (remove deck)

let bust_p (player : player) =
  if player.hand_val > 21 then true else false

let bust_d (dealer : dealer) =
  if dealer.hand_val > 21 then true else false

let rec yes_no
    (deck : deck)
    (total : int)
    (player : player)
    (dealer : dealer) =
  let y_n = read_line () in
  match Command.check_yn y_n with
  | "yes" -> "yes"
  | "no" -> "no"
  | _ ->
      print_string "\nInvalid input, please try again.\n\n";
      print_string "> ";
      yes_no deck total player dealer

let rec parse_input deck total (player : player) (dealer : dealer) =
  let line = read_line () in
  match Command.check_string line with
  | "hit" ->
      (*Empty Deck: Create a newly shuffled deck Draw a card and extract
        the point value Then parse for a hit or stay command *)
      if empty deck then (
        let new_deck = shuffle create in
        let card_drawn = draw_deck new_deck in
        let (new_deck_val : int) = point_add total card_drawn player in
        print_string "> ";
        parse_input new_deck new_deck_val player dealer )
      else
        (*Draw a card and extract point value*)
        let card_drawn = draw_deck deck in
        let new_val = point_add total card_drawn player in
        print_string "> ";
        parse_input (remove deck) new_val player dealer
  | "stay" ->
      print_endline ("Your total value is " ^ string_of_int total ^ ".");
      print_string "The round is over, do you want to continue?";
      print_string "> ";
      let response = yes_no deck total player dealer in
      if response = "yes" then print_endline "Start a new round."
      else print_endline "Goodbye, thanks for playing."
  | _ ->
      print_string "\nInvalid input, please try again.\n\n";
      print_string "> ";
      parse_input deck total player dealer
