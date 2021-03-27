open OUnit2
open Deck

let shuffle_test
    (name : string)
    (deck : Deck.deck)
    (expected_output : Deck.deck) : test =
  name >:: fun _ ->
  assert_equal (sort_deck deck) (sort_deck (shuffle deck))

let deck_tests =
  [
    shuffle_test "Random Shuffle 1" (shuffle create) create;
    shuffle_test "Random Shuffle 2" (shuffle create) create;
  ]

let suite = "test suite for blackjack" >::: List.flatten [ deck_tests ]

let _ = run_test_tt_main suite
