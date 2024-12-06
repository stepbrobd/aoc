open! Core
open! Common

let part_1 path =
  let matrix = get_lines path |> List.map ~f:String.to_list in
  let rows = List.length matrix in
  let cols = List.length (List.hd_exn matrix) in
  let get x y =
    if x >= 0 && x < rows && y >= 0 && y < cols
    then Some (List.nth_exn (List.nth_exn matrix x) y)
    else None
  in
  let check dirs =
    match List.map dirs ~f:(fun (x, y) -> get x y) with
    | Some 'X' :: Some 'M' :: Some 'A' :: Some 'S' :: _ -> true
    | _ -> false
  in
  let dirs =
    [ (* right *)
      (fun x y -> [ x, y; x + 1, y; x + 2, y; x + 3, y ])
    ; (* left *)
      (fun x y -> [ x, y; x - 1, y; x - 2, y; x - 3, y ])
    ; (* down *)
      (fun x y -> [ x, y; x, y + 1; x, y + 2; x, y + 3 ])
    ; (* up *)
      (fun x y -> [ x, y; x, y - 1; x, y - 2; x, y - 3 ])
    ; (* lower right *)
      (fun x y -> [ x, y; x + 1, y + 1; x + 2, y + 2; x + 3, y + 3 ])
    ; (* upper right *)
      (fun x y -> [ x, y; x - 1, y + 1; x - 2, y + 2; x - 3, y + 3 ])
    ; (* lower left *)
      (fun x y -> [ x, y; x + 1, y - 1; x + 2, y - 2; x + 3, y - 3 ])
    ; (* upper left *)
      (fun x y -> [ x, y; x - 1, y - 1; x - 2, y - 2; x - 3, y - 3 ])
    ]
  in
  let cnt = ref 0 in
  for x = 0 to rows - 1 do
    for y = 0 to cols - 1 do
      List.iter dirs ~f:(fun dir -> if check (dir x y) then incr cnt)
    done
  done;
  !cnt |> string_of_int
;;

let part_2 path =
  (* bruh i dont give a fuck *)
  let matrix = get_lines path |> List.map ~f:String.to_list in
  let rows = List.length matrix in
  let cols = List.length (List.hd_exn matrix) in
  let get x y = List.nth_exn (List.nth_exn matrix x) y in
  let check x y =
    Char.equal (get x y) 'A'
    && ((Char.equal (get (x + 1) (y + 1)) 'M' && Char.equal (get (x - 1) (y - 1)) 'S')
        || (Char.equal (get (x + 1) (y + 1)) 'S' && Char.equal (get (x - 1) (y - 1)) 'M')
       )
    && ((Char.equal (get (x + 1) (y - 1)) 'M' && Char.equal (get (x - 1) (y + 1)) 'S')
        || (Char.equal (get (x + 1) (y - 1)) 'S' && Char.equal (get (x - 1) (y + 1)) 'M')
       )
  in
  let cnt = ref 0 in
  for x = 0 to rows - 1 do
    for y = 0 to cols - 1 do
      if x + 1 < rows && x - 1 >= 0 && y + 1 < cols && y - 1 >= 0 && check x y
      then incr cnt
    done
  done;
  !cnt |> string_of_int
;;
