open Common
open Core

let () =
  match Sys.get_argv () |> Array.to_list with
  | [ _; year; day; part ] -> print_endline (dispatch year day part (get_input year day))
  | _ -> failwith "dune exec aoc -- <year> <day> <part>"
;;
