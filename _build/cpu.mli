(**Contains the instruction for the automated player*)

open Text
open Deck
open Player

(**[run_cpu] runs the CPU Game*)
val run_cpu :
  deck -> player -> dealer -> player -> string -> deck * player
