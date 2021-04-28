open OUnit2
open Deck
open Command
open Player

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
  ]

(******* Command Testing *******)
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
  }

let player_j_a =
  {
    hand = [ j; a ];
    hand_val = 21;
    chips = 100;
    bet = 0;
    win_round = 0;
    is_blackjack = true;
  }

let player_j_a_7 =
  {
    hand = [];
    hand_val = 28;
    chips = 100;
    bet = 0;
    win_round = -2;
    is_blackjack = false;
  }

let player_7 =
  {
    hand = [];
    hand_val = 7;
    chips = 100;
    bet = 0;
    win_round = 0;
    is_blackjack = false;
  }

let player_7_a1 =
  {
    hand = [];
    hand_val = 8;
    chips = 100;
    bet = 0;
    win_round = 0;
    is_blackjack = false;
  }

let player_7_4 =
  {
    hand = [];
    hand_val = 11;
    chips = 100;
    bet = 0;
    win_round = 0;
    is_blackjack = false;
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

(******* Round and Main Testing - Need to play the game to see*******)

let suite =
  "test suite for blackjack"
  >::: List.flatten [ deck_tests; command_tests; player_tests ]

let _ = run_test_tt_main suite
