open Text
open Deck
open Player

(**Update how many cards we have seen based on what is in the player's
   hand*)
let rec player_combo
    (player_card : card list)
    (combo : float)
    (ceiling : int) =
  match player_card with
  | [] -> combo
  | h :: t ->
      if List.hd h.point <= ceiling then
        let combo' = combo -. 1. in
        player_combo t combo' ceiling
      else player_combo t combo ceiling

(**Generates number of valid cards that would cause us not to bust*)
let combo num =
  if num >= 10 then float_of_int 10 *. 4. else float_of_int num *. 4.

(**Runs a smart game based on cards seen and probability of busting*)
let rec cpu_smart_run_game
    (deck : deck)
    (cpu : player)
    (dealer : dealer)
    (player : player) =
  if cpu.hand_val > 21 then (
    print_endline "\nThe CPU busted.\n";
    (deck, cpu) )
  else
    let valid_num = 21 - cpu.hand_val in
    let valid_combo = combo valid_num in
    let total_outcomes = 49 - List.length cpu.hand in
    if dealer.hand_val <= valid_num then
      let valid_combo' = valid_combo -. 1. in
      probability_func deck cpu dealer player valid_combo' valid_num
        total_outcomes
    else
      probability_func deck cpu dealer player valid_combo valid_num
        total_outcomes

(**Determines the probability of busting and act accordingly to it*)
and probability_func
    (deck : deck)
    (cpu : player)
    (dealer : dealer)
    (player : player)
    (valid_combo : float)
    (valid_num : int)
    (total_outcomes : int) =
  let valid_combo'' = player_combo player.hand valid_combo valid_num in

  let probability = valid_combo'' /. float_of_int total_outcomes in
  if probability <= 0. then
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\nThe CPU's probability of not busting is: 0"
  else
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ( "\nThe CPU's probability of not busting is:"
      ^ string_of_float probability
      ^ "\n" );
  if probability >= 0.55 then (
    let cpu' = player_update deck cpu in
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\nThe CPU's next card is: \n\n";
    let deck' = remove deck in
    card deck;
    cpu_smart_run_game deck' cpu' dealer player )
  else (deck, cpu)

(**Runs through the necessary steps to start the CPU hand*)
let begin_game
    (deck : deck)
    (cpu : player)
    (dealer : dealer)
    (player : player) =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\nThe CPU's first card is: \n\n";
  print_card (draw deck);
  let cpu_1 = player_update deck cpu in
  let updated_deck2 = remove deck in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\nThe CPU's second card is: \n\n";
  print_card (draw updated_deck2);
  (cpu_1, updated_deck2)

(**Stops drawing at a hard total and determine a string to output and
   CPU state to return*)
let cpu_stop_draw (deck : deck) (cpu : player) =
  if cpu.hand_val = 21 then (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\nThe CPU got Blackjack!\n";
    (deck, cpu) )
  else (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ("\nThe CPU's hand value is " ^ string_of_int cpu.hand_val ^ "\n");
    (deck, cpu) )

(**Runs the apporpriate CPU version*)
let cpu_func
    (updated_deck3 : deck)
    (cpu_2 : player)
    (dealer : dealer)
    (player : player)
    (func : deck -> player -> dealer -> player -> deck * player) =
  let deck_cpu = func updated_deck3 cpu_2 dealer player in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ( "\nThe CPU's hand value is "
    ^ string_of_int (snd deck_cpu).hand_val
    ^ "\n" );
  (deck_cpu : deck * player)

(**Runs the game based on hard totals and Blackjack strategy*)
let rec cpu_run_game
    (deck : deck)
    (cpu : player)
    (dealer : dealer)
    (player : player) =
  if cpu.hand_val > 21 then (
    print_endline "\nThe CPU busted.\n";
    (deck, cpu) )
  else if cpu.hand_val >= 17 then (deck, cpu)
  else if cpu.hand_val >= 13 && cpu.hand_val <= 16 then
    if dealer.hand_val >= 2 && dealer.hand_val <= 6 then (deck, cpu)
    else cpu_next_card deck cpu dealer player
  else if cpu.hand_val = 12 then
    if dealer.hand_val >= 4 && dealer.hand_val <= 6 then (deck, cpu)
    else cpu_next_card deck cpu dealer player
  else cpu_next_card deck cpu dealer player

(**Display's CPU's next card*)
and cpu_next_card deck cpu dealer player =
  let cpu' = player_update deck cpu in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\nThe CPU's next card is: \n";
  let deck' = remove deck in
  card deck;
  cpu_run_game deck' cpu' dealer player

(**Runs the CPU round based on the input level*)
let run_cpu
    (deck : deck)
    (cpu : player)
    (dealer : dealer)
    (player_2 : player)
    (lvl : string) =
  let state' = begin_game deck cpu dealer player_2 in
  let cpu_2 = player_update (snd state') (fst state') in
  let updated_deck3 = remove (snd state') in

  if cpu_2.hand_val >= 17 then cpu_stop_draw deck cpu_2
  else if lvl = "1" then
    cpu_func updated_deck3 cpu_2 dealer player_2 cpu_run_game
  else cpu_func updated_deck3 cpu_2 dealer player_2 cpu_smart_run_game
