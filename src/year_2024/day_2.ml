open! Core
open! Common

let check x =
  is_sorted_custom x ~compare:(fun a b ->
    let diff = abs (a - b) in
    diff >= 1 && diff <= 3)
;;

let part_1 path =
  get_lines path
  |> List.map ~f:of_int_list
  |> List.filter ~f:check
  |> List.length
  |> string_of_int
;;

let part_2 path =
  let valid x =
    List.existsi x ~f:(fun i _ ->
      let l' = List.filteri x ~f:(fun idx _ -> i <> idx) in
      check l')
  in
  get_lines path
  |> List.map ~f:of_int_list
  |> List.filter ~f:valid
  |> List.length
  |> string_of_int
;;
