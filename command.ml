type command = string

let valid_length_1_11 str =
  let str_list = StringLabels.split_on_char ' ' str in
  if List.length str_list <> 1 then "invalid input"
  else
    let format_str = StringLabels.lowercase_ascii str in
    match format_str with
    | "1" -> "1"
    | "11" -> "11"
    | _ -> "invalid input"

let valid_length_yes_no str =
  let str_list = StringLabels.split_on_char ' ' str in
  if List.length str_list <> 1 then "invalid input"
  else
    let format_str = StringLabels.lowercase_ascii str in
    match format_str with
    | "yes" -> "yes"
    | "no" -> "no"
    | _ -> "invalid input"

let valid_length_hit_stay str =
  let str_list = StringLabels.split_on_char ' ' str in
  if List.length str_list <> 1 then "invalid input"
  else
    let format_str = StringLabels.lowercase_ascii str in
    match format_str with
    | "hit" -> "hit"
    | "stay" -> "stay"
    | _ -> "invalid input"

let empty str funct = if str = "" then "empty" else funct str

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
