type card = {
  suite : string;
  rank : string;
  point : int list;
}

type deck = card list

exception RP_Not_Found

exception Empty

let suite_lst = [ "♠"; "♥"; "♦"; "♣" ]

let rank =
  [ "A"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"; "J"; "Q"; "K" ]

let rank_to_points =
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

(** [point_converter] is the point that a specific rank can take on*)
let rec point_converter rank rank_to_points =
  match rank_to_points with
  | h :: t -> if fst h = rank then snd h else point_converter rank t
  | [] -> raise RP_Not_Found

(** [create_rank] creates card out of a certain suite for each rank
    element in the list *)
let rec create_rank suite rank acc =
  match rank with
  | [] -> acc
  | h :: t ->
      let acc =
        { suite; rank = h; point = point_converter h rank_to_points }
        :: acc
      in
      create_rank suite t acc

(** [create_suite] takes in a list of suites and creates cards for each
    suite in the list *)
let rec create_suite suite acc =
  match suite with
  | [] -> acc
  | h :: t ->
      let acc = create_rank h rank acc in
      create_suite t acc

let create = List.rev (create_suite suite_lst [])

let shuffle (deck : deck) =
  let deck_array = Array.of_list deck in
  for i = List.length deck - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = deck_array.(i) in
    deck_array.(i) <- deck_array.(j);
    deck_array.(j) <- temp
  done;
  (Array.to_list deck_array : deck)

let draw deck = List.hd deck

let remove deck = match deck with h :: t -> t | [] -> raise Empty

let empty deck = List.length deck < 0

let header = " _______ "

let print_rank rank = "|   " ^ rank ^ "   |"

let print_suite suite = "|   " ^ suite ^ "   |"

let print_norm = "|       |"

let rec print_list lst =
  match lst with
  | [] -> print_endline ""
  | h :: t ->
      print_endline h;
      print_list t

let print_card_helper suite rank =
  let card_list =
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
  in
  print_list card_list

let print_card (card : card) = print_card_helper card.suite card.rank

let sort_deck deck = List.sort compare deck
