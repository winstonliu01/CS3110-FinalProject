open Deck

let deck_in_round = shuffle create

let command =
  print_endline "Do you want to hit or stay?.\n";
  print_string "> ";
  (*Need to decide what to do for the pattern match*)
  match read_line () with
  | "Hit" -> print_endline "Hit"
  | "Stay" -> print_endline "Stay"
  | _ -> print_endline "Invalid input"

let start_round deck =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nThe round is now starting.";
  print_endline "\nThe deck has been created and shuffled .\n";
  print_endline "\nThe dealer's first card is .\n";
  print_card (draw deck);
  let deck = remove deck in
  print_endline "\nYour cards are .\n";
  print_card (draw deck);
  let deck = remove deck in
  print_card (draw deck);
  command

(** [main ()] starts blackjack. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the Blackjack engine.\n";
  ANSITerminal.print_string [ ANSITerminal.red ]
    "You are starting with 500 chips.\n";
  start_round deck_in_round

(* Execute the game engine. *)
let () = main ()
