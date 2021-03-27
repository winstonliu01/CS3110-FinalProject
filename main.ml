open Deck

let deck_in_round = shuffle create

let start_round deck =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nThe round is now starting.";
  print_endline "\nThe deck has been created and shuffled .\n";
  print_endline "\nThe dealer's first card is .\n";
  print_card (draw deck);
  print_endline "\nYour cards are .\n";
  print_card (draw (remove deck));
  print_card (draw (remove (remove deck)));
  ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the Blackjack engine.\n";
  ANSITerminal.print_string [ ANSITerminal.red ]
    "You are starting with 500 chips.\n";
  start_round deck_in_round

(* Execute the game engine. *)
let () = main ()
