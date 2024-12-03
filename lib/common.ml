open Core

module type Puzzle = sig
  val part_1 : string list -> string
  val part_2 : string list -> string
end

let get_lines path =
  try
    Some
      (In_channel.with_file path ~f:(fun in_channel -> In_channel.input_lines in_channel))
  with
  | _ -> None
;;

let get_input year day =
  match get_lines (Printf.sprintf "src/year_%s/day_%s.txt" year day) with
  | Some lines -> lines
  | None -> []
;;
