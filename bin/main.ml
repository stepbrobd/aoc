open! Core
open! Common
open Curl

let download year day =
  let path = get_input_path year day in
  let url = sprintf "https://adventofcode.com/%s/day/%s/input" year day in
  let curl = Curl.init () in
  let buffer = Buffer.create (1024 * 16) in
  set_url curl url;
  set_cookie curl (sprintf "session=%s" (Sys.getenv_exn "COOKIE"));
  set_writefunction curl (fun s ->
    Buffer.add_string buffer s;
    String.length s);
  perform curl;
  cleanup curl;
  Out_channel.write_all path ~data:(Buffer.contents buffer);
  ()
;;

let dispatch year day part path =
  let module_for year day =
    let open Aoc in
    match year with
    | "2024" ->
      (match day with
       | "1" -> Some (module Year_2024.Day_1 : Puzzle)
       | "2" -> Some (module Year_2024.Day_2 : Puzzle)
       | "3" -> Some (module Year_2024.Day_3 : Puzzle)
       | "4" -> Some (module Year_2024.Day_4 : Puzzle)
       | "5" -> Some (module Year_2024.Day_5 : Puzzle)
       | "6" -> Some (module Year_2024.Day_6 : Puzzle)
       | "7" -> Some (module Year_2024.Day_7 : Puzzle)
       | "8" -> Some (module Year_2024.Day_8 : Puzzle)
       | "9" -> Some (module Year_2024.Day_9 : Puzzle)
       | "10" -> Some (module Year_2024.Day_10 : Puzzle)
       | "11" -> Some (module Year_2024.Day_11 : Puzzle)
       | "12" -> Some (module Year_2024.Day_12 : Puzzle)
       | "13" -> Some (module Year_2024.Day_13 : Puzzle)
       | "14" -> Some (module Year_2024.Day_14 : Puzzle)
       | "15" -> Some (module Year_2024.Day_15 : Puzzle)
       | "16" -> Some (module Year_2024.Day_16 : Puzzle)
       | "17" -> Some (module Year_2024.Day_17 : Puzzle)
       | "18" -> Some (module Year_2024.Day_18 : Puzzle)
       | "19" -> Some (module Year_2024.Day_19 : Puzzle)
       | "20" -> Some (module Year_2024.Day_20 : Puzzle)
       | "21" -> Some (module Year_2024.Day_21 : Puzzle)
       | "22" -> Some (module Year_2024.Day_22 : Puzzle)
       | "23" -> Some (module Year_2024.Day_23 : Puzzle)
       | "24" -> Some (module Year_2024.Day_24 : Puzzle)
       | "25" -> Some (module Year_2024.Day_25 : Puzzle)
       | _ -> None)
    | _ -> None
  in
  match module_for year day with
  | Some (module M : Puzzle) ->
    (match part with
     | "1" -> M.part_1 path
     | "2" -> M.part_2 path
     | _ -> sprintf "part %s not valid" part)
  | None -> sprintf "dispatch failed for year %s day %s" year day
;;

let () =
  match Sys.get_argv () |> Array.to_list with
  | [ _; year; day; part ] ->
    let () = download year day in
    print_endline (dispatch year day part (get_input_path year day))
  | _ -> failwith "dune exec aoc -- <year> <day> <part>"
;;
