type command = string

exception Empty

exception Malformed

let check_verb str =
  match str with
  | "hit" -> "hit"
  | "stay" -> "stay"
  | _ -> "invalid input"

let valid_length str =
  let str_list = StringLabels.split_on_char ' ' str in
  if List.length str_list <> 1 then "invalid"
  else
    let format_str = StringLabels.lowercase_ascii str in
    check_verb format_str

let empty str = if str = "" then "empty" else valid_length str

let deblank str = StringLabels.trim str

let check_string str =
  let trim_str = deblank str in
  empty trim_str
