open OUnit2
open Deck
open Command

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

let player_tests = []

(******* Round and Main Testing - Need to play the game to see*******)

let suite =
  "test suite for blackjack"
  >::: List.flatten [ deck_tests; command_tests ]

let _ = run_test_tt_main suite
