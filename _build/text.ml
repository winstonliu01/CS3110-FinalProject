let welcome_string =
  "\n\n\
   Welcome to the Blackjack engine.\n\
   You are starting with 100 chips."

let start_round_string =
  "\n\n\
   The round is now starting. \n\
   The deck has been created and shuffled.\n"

let dealer_card1_string = "\nThe dealer's first card is: \n"

let new_round_string = "\nA new round is starting, get ready!\n"

let invalid_input = "Invalid input. Please try again."

let dealer_remaining_card =
  "\nThe dealer's hidden card and remaining cards are:\n"

let more_round = "Would you like to play another round? \n"

let win = "You won the round! \n"

let draw = "The round is a draw. \n"

let bust = "Sorry, you busted! \n"

let loss_dealer = "You lost the round to the dealer. \n"

let bankrupt = "\nSorry, you went bankrupt!\n"

let level () =
  print_endline
    "\n\
    \ There are two levels - Level 1 and Level 2. Which level do you \
     prefer? Please enter '1' or '2'. \n";
  print_string "> "

let empty_print () =
  print_endline "\nEmpty input, please try again. \n";
  print_string "> "

let invalid_print () =
  print_endline "\nInvalid input, please try again. \n";
  print_string "> "

let h_or_s () =
  print_endline "\nDo you want to hit or stay?\n";
  print_string "> "

let place_bets () =
  print_endline "\nPlease place your bet.\n";
  print_string "> "

let all_in () =
  print_endline "\nAre you sure you want to go all in?\n";
  print_string "> "
