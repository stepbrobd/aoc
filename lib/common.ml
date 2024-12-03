open Core

module type Puzzle = sig
  val part_1 : string list -> string
  val part_2 : string list -> string
end

let get_lines path =
  In_channel.with_file path ~f:(fun in_channel -> In_channel.input_lines in_channel)
;;

let get_input year day = get_lines (sprintf "src/year_%s/day_%s.txt" year day)

let get_module year day =
  let open Aoc in
  match year, day with
  | "2024", "1" -> Some (module Year_2024.Day_1 : Puzzle)
  | "2024", "2" -> Some (module Year_2024.Day_2 : Puzzle)
  | _ -> None
;;

let dispatch year day part input =
  match get_module year day with
  | Some (module M : Puzzle) ->
    (match part with
     | "1" -> M.part_1 input
     | "2" -> M.part_2 input
     | _ -> sprintf "part %s not valid" part)
  | None -> sprintf "dispatch failed for year %s day %s" year day
;;
