open OUnit2
open Deck
open Command
open Player

(*Testing Approach*)

(* Since our final project was a game, we played it manually to
   determine if the behavior is what we expected. However, we used
   automated testing heavily in the beginning sprints to test the
   correctness of our set up. Within the Deck module, we used size to
   test whether we were removing cards correctly and if shuffling it
   would have any effect on it. Additionally, we removed cards from a
   sorted deck to ensure that the proper cards were being drawn. In the
   Command module, we wrote automated tests as well for the various
   functions. This module allowed us to test more in depth since it did
   not require us to create values before hand. So, we tested for valid
   inputs as well as invalid inputs such as malformed and empty inputs.
   For these two modules, we used a glassbox approach as we analyzed
   places where could be potential entry points and tested them
   directly. The player module was more difficult to test automatically
   because it required us to constantly create player and card values.
   Therefore, we weren't able to utilize a glassbox approach and used a
   random spot check here. So although we did write some OUnit tests, it
   was around here when we started to rely on manual testing. This is
   the same case for the Cpu, Main,and Round modules. By playing the
   game, we believe that we could prove our program's correctness. Even
   now, we have print statements for the user to display the total.
   Doing so when we tested, we were able to see if points were being
   added correctly. Furthermore, we saw whether the next round was being
   started with the right states as an update was given that told us how
   many chips the CPU and player had. Lastly, the behavior for busting
   and winning/losing the game will invoke a different statement to be
   printed. By running through many games, we noticed that the system
   seem to be displaying correct value and outputs to console. However,
   we were unable to perfectly confirm this as the setting for each
   round is different given our shuffle function. But, we strongly
   believe that the behavior is correct based on what we have seen
   despite possible edge cases popping up. In summary, we used OUnit
   testing early on to see our set up was valid and manually played the
   game which tested our entire program. *)

(******* Deck Testing *******)

let shuffle_test
    (name : string)
    (deck : Deck.deck)
    (expected_output : Deck.deck) : test =
  name >:: fun _ ->
  assert_equal (sort_deck expected_output) (sort_deck (shuffle deck))

let empty_test
    (name : string)
    (deck : Deck.deck)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (empty deck)

let draw_test
    (name : string)
    (deck : Deck.deck)
    (expected_output : Deck.card) : test =
  name >:: fun _ -> assert_equal expected_output (draw deck)

let point_test
    (name : string)
    (card : Deck.card)
    (expected_output : int list) : test =
  name >:: fun _ -> assert_equal expected_output (point card)

let size_test (name : string) (deck : Deck.deck) (expected_output : int)
    : test =
  name >:: fun _ -> assert_equal expected_output (size deck)

let deck_tests =
  [
    size_test "Full Deck Size Init" create 52;
    size_test "Full Deck Size Shuffled" (shuffle create) 52;
    size_test "One card removed" (remove create) 51;
    size_test "Two cards removed" (remove (remove create)) 50;
    size_test "Three cards removed" (remove (remove (remove create))) 49;
    shuffle_test "Random Shuffle 1 - Cards still present"
      (shuffle create) create;
    shuffle_test "Random Shuffle 2 - Cards still present"
      (shuffle create) create;
    shuffle_test "Random Shuffle 3 - Cards still present"
      (shuffle create) create;
    shuffle_test "Random Shuffle 4 - Cards still present"
      (shuffle create) create;
    ( "shuffling changes the deck" >:: fun _ ->
      assert (not (shuffle create = shuffle create)) );
    empty_test "Not empty" create false;
    empty_test "Not empty" (shuffle create) false;
    point_test "First card" (draw create) [ 1; 11 ];
    point_test "Second card" (draw (remove create)) [ 2 ];
    point_test "Third card" (draw (remove (remove create))) [ 3 ];
  ]

(******* Command Testing *******)

(*Pretty Printer for Test Cases*)
(* let pp_string s = "\"" ^ s ^ "\"" *)

let check_hit_stay_test
    (name : string)
    (input : string)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (check_hit_stay input)

let check_yes_no_test
    (name : string)
    (input : string)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (check_yes_no input)

let check_1_11_test
    (name : string)
    (input : string)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (check_1_11 input)

let check_bet_test
    (name : string)
    (input : string)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (check_bet input)

let command_tests =
  [
    check_hit_stay_test "Already proper hit" "hit" "hit";
    check_hit_stay_test "Already proper stay" "stay" "stay";
    check_hit_stay_test "Capitalized hit" "hIt" "hit";
    check_hit_stay_test "Capitalized stay" "sTAY" "stay";
    check_hit_stay_test "Spaced hit" "    hit   " "hit";
    check_hit_stay_test "Spaced stay" "   stay     " "stay";
    check_hit_stay_test "Capitalized + spaced hit" "    HIT   " "hit";
    check_hit_stay_test "Capitalized + spaced stay" "   STAY     "
      "stay";
    check_hit_stay_test "empty string" "  " "empty";
    check_hit_stay_test "malformed string" "jiberish" "invalid input";
    check_hit_stay_test "malformed string" "h i t" "invalid input";
    check_yes_no_test "Already proper yes" "yes" "yes";
    check_yes_no_test "Already proper no" "no" "no";
    check_yes_no_test "Capitalized yes" "YES" "yes";
    check_yes_no_test "Capitalized no" "nO" "no";
    check_yes_no_test "Spaced yes" "    yes      " "yes";
    check_yes_no_test "Spaced no" "no     " "no";
    check_yes_no_test "empty string" "  " "empty";
    check_yes_no_test "malformed string" "ocaml" "invalid input";
    check_1_11_test "Already proper 1" "1" "1";
    check_1_11_test "Already proper 11" "11" "11";
    check_1_11_test "empty string" "     " "empty";
    check_1_11_test "malformed string" "22" "invalid input";
    check_bet_test "Valid Bet" "22" "22";
    check_bet_test "Valid Bet" "0" "0";
    check_bet_test "Non-integer" "abcd" "invalid input";
    check_bet_test "Non-integer" "2r3" "invalid input";
  ]

(******* Player Testing *******)

let point_add_player_test
    (name : string)
    (total : int)
    (card : card)
    (player : player)
    (expected_output : player) : test =
  name >:: fun _ ->
  assert_equal expected_output (point_add_player total card player)

let black_jack_checker_test
    (name : string)
    (player : player)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (black_jack_checker player)

let bust_checker_player_test
    (name : string)
    (player : player)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (bust_checker_player player)

let j = { suite = "♠"; rank = "J"; point = [ 10 ] }

let a = { suite = "♠"; rank = "A"; point = [ 11 ] }

let a1 = { suite = "♠"; rank = "A"; point = [ 11 ] }

let num7 = { suite = "♠"; rank = "7"; point = [ 7 ] }

let num4 = { suite = "♠"; rank = "4"; point = [ 4 ] }

let player_j =
  {
    hand = [];
    hand_val = 10;
    chips = 100;
    bet = 0;
    win_round = 0;
    is_blackjack = false;
    side_bet = 0;
    is_cpu = false;
  }

let player_j_a =
  {
    hand = [ j; a ];
    hand_val = 21;
    chips = 100;
    bet = 0;
    win_round = 0;
    is_blackjack = true;
    side_bet = 0;
    is_cpu = false;
  }

let player_j_a_7 =
  {
    hand = [];
    hand_val = 28;
    chips = 100;
    bet = 0;
    win_round = -2;
    is_blackjack = false;
    side_bet = 0;
    is_cpu = false;
  }

let player_7 =
  {
    hand = [];
    hand_val = 7;
    chips = 100;
    bet = 0;
    win_round = 0;
    is_blackjack = false;
    side_bet = 0;
    is_cpu = false;
  }

let player_7_a1 =
  {
    hand = [];
    hand_val = 8;
    chips = 100;
    bet = 0;
    win_round = 0;
    is_blackjack = false;
    side_bet = 0;
    is_cpu = false;
  }

let player_7_4 =
  {
    hand = [];
    hand_val = 11;
    chips = 100;
    bet = 0;
    win_round = 0;
    is_blackjack = false;
    side_bet = 0;
    is_cpu = false;
  }

let player_tests =
  [
    point_add_player_test "One card added 0" 0 j player_init player_j;
    point_add_player_test "One card added 7" 7 num4 player_7 player_7_4;
    black_jack_checker_test "No Blackjack" player_init false;
    black_jack_checker_test "No Blackjack" player_7_a1 false;
    black_jack_checker_test "Blackjack" player_j_a true;
    bust_checker_player_test "No Bust" player_j_a false;
    bust_checker_player_test "No Bust" player_7_a1 false;
    bust_checker_player_test "Busted" player_j_a_7 true;
  ]

let suite =
  "test suite for blackjack"
  >::: List.flatten [ deck_tests; command_tests; player_tests ]

let _ = run_test_tt_main suite
