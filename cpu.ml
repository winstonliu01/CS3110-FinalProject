open Text
open Deck
open Player

let card deck =
  let c = draw deck in
  print_card c

let cpu_update (deck : deck) (player : player) =
  let player_card = draw deck in
  let updated_player =
    point_add_player player.hand_val player_card player
  in
  let player =
    {
      hand = player.hand @ [ player_card ];
      hand_val = updated_player.hand_val;
      chips = player.chips;
      bet = player.bet;
      win_round = player.win_round;
      is_blackjack = player.is_blackjack;
      side_bet = player.side_bet;
      is_cpu = player.is_cpu;
    }
  in
  player

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

let combo num =
  if num >= 10 then float_of_int 10 *. 4. else float_of_int num *. 4.

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
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("\nThe CPU's probability is:" ^ string_of_float probability);
  if probability >= 0.55 then (
    let cpu' = cpu_update deck cpu in
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\nThe CPU's next card is: \n";
    let deck' = remove deck in
    card deck;
    cpu_smart_run_game deck' cpu' dealer player )
  else (deck, cpu)

let cpu_smart_game
    (deck : deck)
    (cpu : player)
    (dealer : dealer)
    (player : player) =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\nThe CPU's first card is: \n";
  print_card (draw deck);
  let cpu_1 = cpu_update deck cpu in
  let updated_deck2 = remove deck in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\nThe CPU's second card is: \n";
  print_card (draw updated_deck2);
  let cpu_2 = cpu_update updated_deck2 cpu_1 in
  let updated_deck3 = remove updated_deck2 in

  if cpu_2.hand_val >= 17 then
    if cpu_2.hand_val = 21 then (
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "\nThe CPU got Blackjack!\n";
      (deck, cpu_2) )
    else (
      ANSITerminal.print_string [ ANSITerminal.blue ]
        ( "\nThe CPU's hand value is "
        ^ string_of_int cpu_2.hand_val
        ^ "\n" );
      (deck, cpu_2) )
  else
    let deck_cpu =
      cpu_smart_run_game updated_deck3 cpu_2 dealer player
    in
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ( "\nThe CPU's hand value is "
      ^ string_of_int (snd deck_cpu).hand_val
      ^ "\n" );
    deck_cpu

let rec cpu_run_game (deck : deck) (cpu : player) (dealer : dealer) =
  if cpu.hand_val > 21 then (
    print_endline "\nThe CPU busted.\n";
    (deck, cpu) )
  else if cpu.hand_val >= 17 then (deck, cpu)
  else if cpu.hand_val >= 13 && cpu.hand_val <= 16 then (
    if dealer.hand_val >= 2 && dealer.hand_val <= 6 then (deck, cpu)
    else
      let cpu' = cpu_update deck cpu in
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "\nThe CPU's next card is: \n";
      let deck' = remove deck in
      card deck;
      cpu_run_game deck' cpu' dealer )
  else if cpu.hand_val = 12 then (
    if dealer.hand_val >= 4 && dealer.hand_val <= 6 then (deck, cpu)
    else
      let cpu' = cpu_update deck cpu in
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "\nThe CPU's next card is: \n";
      let deck' = remove deck in
      card deck;
      cpu_run_game deck' cpu' dealer )
  else
    let cpu' = cpu_update deck cpu in
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\nThe CPU's next card is: \n";
    let deck' = remove deck in
    card deck;
    cpu_run_game deck' cpu' dealer

let cpu_game (deck : deck) (cpu : player) (dealer : dealer) =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\nThe CPU's first card is: \n";
  print_card (draw deck);
  let cpu_1 = cpu_update deck cpu in
  let updated_deck2 = remove deck in
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\nThe CPU's second card is: \n";
  print_card (draw updated_deck2);
  let cpu_2 = cpu_update updated_deck2 cpu_1 in
  let updated_deck3 = remove updated_deck2 in

  if cpu_2.hand_val >= 17 then
    if cpu_2.hand_val = 21 then (
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "\nThe CPU got Blackjack!\n";
      (deck, cpu_2) )
    else (
      ANSITerminal.print_string [ ANSITerminal.blue ]
        ( "\nThe CPU's hand value is "
        ^ string_of_int cpu_2.hand_val
        ^ "\n" );
      (deck, cpu_2) )
  else
    let deck_cpu = cpu_run_game updated_deck3 cpu_2 dealer in
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ( "\nThe CPU's hand value is "
      ^ string_of_int (snd deck_cpu).hand_val
      ^ "\n" );
    deck_cpu

let run_cpu
    (deck : deck)
    (cpu : player)
    (dealer : dealer)
    (player_2 : player)
    (lvl : string) =
  if lvl = "1" then cpu_game deck cpu dealer
  else cpu_smart_game deck cpu dealer player_2
