(** Stores long messages outputted in our system*)

(**[welcome_string] is what is printed when the user opens the program *)
val welcome_string : string

(**[start_round_string] is what is printed to inform the user a new
   round is starting*)
val start_round_string : string

(**[dealer_card1_string] tells you what the dealer's first card is*)
val dealer_card1_string : string

(**[new_round_string] is what is printed to inform the user a new round
   is starting*)
val new_round_string : string

(**[invalid_input] prompts the user to enter a valid input *)
val invalid_input : string

(**[dealer_remaining_card] prints the intro to revealing the dealer's
   card *)
val dealer_remaining_card : string

(**[more_round] is the string asking user if they want to play another
   round *)
val more_round : string

(**[win] is the string telling the user they won the round *)
val win : string

(**[draw] is the string telling the user the round was a draw *)
val draw : string

(**[bust] is the string telling the user they busted *)
val bust : string

(**[loss_dealer] is the string telling the user they lost to the dealer *)
val loss_dealer : string

(**[bankrupt] is the string telling the user they went bankrupt *)
val bankrupt : string

(**[bankrupt] is the string asking the user what level they want the CPU
   to be *)
val level : unit -> unit

(**[empty_print] outputs to the user they've inputted an empty input *)
val empty_print : unit -> unit

(**[invalid_print] outputs to the user they've inputted an invalid input *)
val invalid_print : unit -> unit

(**[h_or_s] prompts the user if they want to hit or stay*)
val h_or_s : unit -> unit

(**[place_bets] prompts the user to place their bet*)
val place_bets : unit -> unit

(**[all_in] asks the user if they want to go all in*)
val all_in : unit -> unit
