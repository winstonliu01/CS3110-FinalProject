type card = {
  suite : string;
  rank : string;
  point : int list;
}

type deck = card list

(**Raised if a rank is not matched to a point*)
exception RP_Not_Found

(**Raised if one tries to remove an empty deck*)
exception Empty

(**List of possible suites*)
let suite_lst = [ "♠"; "♥"; "♦"; "♣" ]

(**List of possible ranks*)
let rank =
  [ "A"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"; "J"; "Q"; "K" ]

(**Matches rank to point value*)
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

(**Initialize the Random generator so each time a different number is
   generated*)
let _ = Random.self_init ()

let shuffle (deck : deck) =
  let deck_array = Array.of_list deck in
  for pos = List.length deck - 1 downto 1 do
    let j = Random.int (pos + 1) in
    let temp = deck_array.(pos) in
    deck_array.(pos) <- deck_array.(j);
    deck_array.(j) <- temp
  done;
  let deck_lst = (Array.to_list deck_array : deck) in
  deck_lst

let draw deck = List.hd deck

let remove deck = match deck with h :: t -> t | [] -> raise Empty

let size deck = List.length deck

let empty deck = List.length deck = 0

let point card = card.point

let sort_deck deck = List.sort compare deck

(**Top and bottom of the card design*)
let header = " _______ "

(**Side for cards - rank row*)
let print_rank rank is_ten =
  if is_ten = false then "|   " ^ rank ^ "   |"
  else "|  " ^ rank ^ "   |"

(**Side for cards - suite row*)
let print_suite suite is_ten =
  if is_ten = false then "|   " ^ suite ^ "   |"
  else "|   " ^ suite ^ "   |"

(**Side for cards - normal row*)
let print_norm is_ten =
  if is_ten = false then "|       |" else "|       |"

(** Prints the list that contains each row of our card*)
let rec print_list lst =
  match lst with
  | [] -> print_endline ""
  | h :: t ->
      print_endline h;
      print_list t

(**Card layout*)
let print_card_helper suite rank =
  if rank = "10" then
    let card_list =
      [
        header;
        print_rank rank true;
        print_norm true;
        print_norm true;
        print_suite suite true;
        print_norm true;
        print_norm true;
        print_rank rank true;
        header;
      ]
    in
    print_list card_list
  else
    let card_list =
      [
        header;
        print_rank rank false;
        print_norm false;
        print_norm false;
        print_suite suite false;
        print_norm false;
        print_norm false;
        print_rank rank false;
        header;
      ]
    in
    print_list card_list

let print_card (card : card) = print_card_helper card.suite card.rank
