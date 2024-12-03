open! Core

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

(* space separated string to a list of strings *)
(* String *)
let of_string_list s =
  String.split_on_chars s ~on:[ ' ' ] |> List.filter ~f:(fun x -> String.length x > 0)
;;

(* space separated string to a list of ints *)
(* String *)
let of_int_list s = of_string_list s |> List.map ~f:Int.of_string

(* space separated string to a pair of ints (strict two column) *)
(* String *)
let of_int_pair s =
  of_int_list s
  |> function
  | [ a; b ] -> a, b
  | _ -> failwith "invalid format"
;;

(* sort string list with two columns and convert to in pairs *)
(* e.g. ["2 4"; "1 2 "] -> [1; 2], [2; 4] *)
(* List *)
let of_sorted_two_column_int_list input =
  List.map ~f:of_int_pair input
  |> List.unzip
  |> fun (a, b) -> List.sort a ~compare:Int.compare, List.sort b ~compare:Int.compare
;;

(* check if given list is in increasing or decreasing order, with additional constraint between elements *)
(* List *)
let is_sorted_custom list ~compare =
  match list with
  | [] | [ _ ] -> true
  | hd :: tl ->
    let rec aux prev incr decr = function
      | [] -> incr || decr
      | x :: xs ->
        aux
          x
          (compare prev x && incr && prev <= x)
          (compare prev x && decr && prev >= x)
          xs
    in
    aux hd true true tl
;;
