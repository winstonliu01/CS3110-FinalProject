let start_round = failwith "Unimplemented"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the Blackjack engine.\n";
  ANSITerminal.print_string [ ANSITerminal.red ]
    "You are starting with 500 chips.\n";
  ()

(* Execute the game engine. *)
let () = main ()
