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
let command_test
    (name : string)
    (input : string)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (check_string input)

let command_tests =
  [
    command_test "Already proper hit" "hit" "hit";
    command_test "Already proper stay" "stay" "stay";
    command_test "Capitalized hit" "hIt" "hit";
    command_test "Capitalized stay" "sTAY" "stay";
    command_test "Spaced hit" "    hit   " "hit";
    command_test "Spaced stay" "   stay     " "stay";
    command_test "Capitalized + spaced hit" "    HIT   " "hit";
    command_test "Capitalized + spaced stay" "   STAY     " "stay";
    ( "empty string" >:: fun _ ->
      assert_raises Empty (fun () -> check_string " ") );
    ( "bad string" >:: fun _ ->
      assert_raises Malformed (fun () -> check_string "jiberish") );
    ( "bad string" >:: fun _ ->
      assert_raises Malformed (fun () -> check_string "h i t") );
  ]

let suite =
  "test suite for blackjack"
  >::: List.flatten [ deck_tests; command_tests ]

let _ = run_test_tt_main suite
