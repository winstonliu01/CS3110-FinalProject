(**Checks if the string is a valid ace amount*)
let valid_length_1_11 str =
  let str_list = StringLabels.split_on_char ' ' str in
  if List.length str_list <> 1 then "invalid input"
  else
    let format_str = StringLabels.lowercase_ascii str in
    match format_str with
    | "1" -> "1"
    | "11" -> "11"
    | _ -> "invalid input"

(**Checks if the string is a valid side bet*)
let valid_length_1_2_3 str =
  let str_list = StringLabels.split_on_char ' ' str in
  if List.length str_list <> 1 then "invalid input"
  else
    let format_str = StringLabels.lowercase_ascii str in
    match format_str with
    | "1" -> "1"
    | "2" -> "2"
    | "3" -> "3"
    | _ -> "invalid input"

(**Checks if the string is a valid command*)
let valid_length_yes_no str =
  let str_list = StringLabels.split_on_char ' ' str in
  if List.length str_list <> 1 then "invalid input"
  else
    let format_str = StringLabels.lowercase_ascii str in
    match format_str with
    | "yes" -> "yes"
    | "no" -> "no"
    | _ -> "invalid input"

(**Checks if the string is a valid command*)
let valid_length_hit_stay str =
  let str_list = StringLabels.split_on_char ' ' str in
  if List.length str_list <> 1 then "invalid input"
  else
    let format_str = StringLabels.lowercase_ascii str in
    match format_str with
    | "hit" -> "hit"
    | "stay" -> "stay"
    | _ -> "invalid input"

(**Checks if the string is a valid CPU level*)
let valid_length_level str =
  let str_list = StringLabels.split_on_char ' ' str in
  if List.length str_list <> 1 then "invalid input"
  else
    let format_str = StringLabels.lowercase_ascii str in
    match format_str with
    | "1" -> "1"
    | "2" -> "2"
    | _ -> "invalid input"

(**Checks if the string is a valid bet input*)
let valid_bet str =
  try string_of_int (int_of_string str)
  with Failure _ -> "invalid input"

(**Checks if the string that should contain an integer is empty*)
let empty_int str = if str = "" then "empty" else valid_bet str

(**Checks if the string is empty*)
let empty str funct = if str = "" then "empty" else funct str

(**Removes leading and trailing spaces from string*)
let deblank str = StringLabels.trim str

let check_1_11 str =
  let trim_str = deblank str in
  empty trim_str valid_length_1_11

let check_yes_no str =
  let trim_str = deblank str in
  empty trim_str valid_length_yes_no

let check_hit_stay str =
  let trim_str = deblank str in
  empty trim_str valid_length_hit_stay

let check_bet str =
  let trim_str = deblank str in
  empty_int trim_str

let check_side_bet str =
  let trim_str = deblank str in
  empty trim_str valid_length_1_2_3

let check_level str =
  let trim_str = deblank str in
  empty trim_str valid_length_level
