type t = {
  suite : string;
  rank : string;
  point : int list;
}

type deck = t list

exception End_of_RP

let suite_lst = [ "♠"; "♥"; "♦"; "♣" ]

let rank =
  [ "A"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"; "J"; "Q"; "K" ]

let rank_points =
  [
    ("A", [ 1; 11 ]);
    ("2", [ 2 ]);
    ("3", [ 3 ]);
    ("4", [ 4 ]);
    ("5", [ 5 ]);
    ("6", [ 6 ]);
    ("7", [ 7 ]);
    ("8", [ 8 ]);
    ("9", [ 9 ]);
    ("10", [ 10 ]);
    ("J", [ 10 ]);
    ("Q", [ 10 ]);
    ("K", [ 10 ]);
  ]

let rec point_converter rank rank_points =
  match rank_points with
  | h :: t -> if fst h = rank then snd h else point_converter rank t
  | [] -> raise End_of_RP

let rec create_rank suite rank acc =
  match rank with
  | [] -> acc
  | h :: t ->
      let acc =
        { suite; rank = h; point = point_converter h rank_points }
        :: acc
      in
      create_rank suite t acc

let rec create_suite suite acc =
  match suite with
  | [] -> acc
  | h :: t ->
      let acc = create_rank h rank acc in
      create_suite t acc

let create = List.rev (create_suite suite_lst [])

(*Code credit belongs to Jeffrey Scofield.
  https://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml

  Check if it is AI to use someone else's method *)

(* ToDo - This current shuffle is the same each time the program runs,
   so it isn't truly randomnized. Need to fix this *)
let shuffle deck =
  let nd = List.map (fun c -> (Random.bits (), c)) deck in
  let sond = List.sort compare nd in
  List.map snd sond

let draw deck = List.hd deck

let header = " ______ "

let print_rank rank = "|   " ^ rank ^ "   |"

let print_suite suite = "|   " ^ suite ^ "   |"

let print_norm = "|      |"

(*Style needs work*)
let print_card_helper suite rank =
  [
    header;
    print_rank rank;
    print_norm;
    print_norm;
    print_suite suite;
    print_norm;
    print_norm;
    print_rank rank;
    header;
  ]

let print_card (card : t) = print_card_helper card.suite card.rank
