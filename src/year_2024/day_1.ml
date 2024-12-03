open Core
open Common

let part_1 path =
  let input = get_lines path in
  let l, r = of_sorted_two_column_int_list input in
  (*
     let () =
     List.iter l ~f:(fun x ->
     Out_channel.output_string stdout (Int.to_string x);
     print_string " ")
     in
  *)
  List.fold2_exn l r ~init:0 ~f:(fun acc a b -> acc + abs (a - b)) |> string_of_int
;;

let part_2 path =
  let input = get_lines path in
  let l, r = of_sorted_two_column_int_list input in
  List.fold l ~init:0 ~f:(fun acc x -> acc + (x * List.count r ~f:(Int.equal x)))
  |> string_of_int
;;
