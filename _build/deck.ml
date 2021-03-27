type t = {
  suite : string;
  rank : string;
  point : int list;
}

type deck = t list

let suite_lst = [ "♠"; "♥"; "♦"; "♣" ]

let rank =
  [
    "A";
    "1";
    "2";
    "3";
    "4";
    "5";
    "6";
    "7";
    "8";
    "9";
    "10";
    "J";
    "Q";
    "K";
  ]

let point_converter rank = failwith "Unimplemented"

let rec create_rank suite rank acc =
  match rank with
  | [] -> acc
  | h :: t ->
      let acc = { suite; rank = h; point = [ 0 ] } :: acc in
      create_rank suite t acc

let rec create_suite suite acc =
  match suite with
  | [] -> acc
  | h :: t ->
      let acc = create_rank h rank acc in
      create_suite t acc

let create = [ { suite = ""; rank = ""; point = [ 9 ] } ]

(* create_suite suite_lst [] *)

let shuffle = failwith "Unimplemented"

let draw = failwith "Unimplemented"

let print_card = failwith "Unimplemented"
