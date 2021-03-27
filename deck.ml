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

let shuffle = failwith "Unimplemented"

let draw = failwith "Unimplemented"

let print_card = failwith "Unimplemented"
