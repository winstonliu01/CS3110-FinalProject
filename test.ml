open OUnit2

(* Remember to open up each file independently*)
(* In case we need set-like lists, we can use helper functions*)

(*Insert test names here*)
let suite = "Test suite for Blackjack Game" >::: List.flatten []

let _ = run_test_tt_main suite
