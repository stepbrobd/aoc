open Core

module type Puzzle = sig
  val part_1 : string -> string
  val part_2 : string -> string
end

let get_input_path year day = Printf.sprintf "src/year_%s/day_%s.txt" year day

let get_string path =
  try In_channel.read_all path with
  | _ -> ""
;;

let get_lines path =
  try
    In_channel.with_file path ~f:(fun in_channel -> In_channel.input_lines in_channel)
  with
  | _ -> []
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
