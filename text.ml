let welcome_string =
  "\n\n\
   Welcome to the Blackjack engine.\n\
   You are starting with 100 chips."

let start_round_string =
  "\n\n\
   The round is now starting. \n\
   The deck has been created and shuffled.\n\
  \ \n"

let dealer_card1_string = "\nThe dealer's first card is: \n"

let new_round_string = "\nA new round is starting, get ready!\n"

let invalid_input = "Invalid action. Please try again."

let dealer_remaining_card =
  "\nThe dealer's hidden card and remaining cards are:\n"

let empty_print () =
  print_endline "\nEmpty input, please try again. \n";
  print_string "> "

let invalid_print () =
  print_endline "\nInvalid input, please try again. \n";
  print_string "> "

let h_or_s () =
  print_endline "\n\nDo you want to hit or stay?\n";
  print_string "> "
