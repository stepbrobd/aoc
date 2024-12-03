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

let of_int_pair s =
  String.split_on_chars s ~on:[ ' ' ]
  |> List.filter ~f:(fun x -> String.length x > 0)
  |> List.map ~f:Int.of_string
  |> function
  | [ a; b ] -> a, b
  | _ -> failwith "invalid format"
;;

let of_sorted_two_column_int_list input =
  List.map ~f:of_int_pair input
  |> List.unzip
  |> fun (a, b) -> List.sort a ~compare:Int.compare, List.sort b ~compare:Int.compare
;;
