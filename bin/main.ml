open Common
open Core

module type DayModule = sig
  val part1 : string -> string
  val part2 : string -> string
end

let () =
  let args = List.drop (Sys.get_argv () |> Array.to_list) 1 in
  match args with
  | [ year; day; part; input ] ->
    load_module year day;
    let module M = (val (Obj.magic () : (module DayModule))) in
    let result =
      match part with
      | "1" -> M.part1 input
      | "2" -> M.part2 input
      | _ -> error_and_exit "invalid part (use '1' or '2')"
    in
    Printf.printf "result: %s\n" result
  | _ -> error_and_exit "usage: <program> <year> <day> <part> <input>"
;;
