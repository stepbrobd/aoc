open! Core
open! Common

let part_1 path =
  let txt = get_string path in
  let rec solve acc num1 num2 prev rest =
    match rest with
    | "" -> acc
    | _ ->
      let next = String.drop_prefix rest 1 in
      let num n = Char.to_int n - Char.to_int '0' in
      (match rest.[0], prev with
       | 'm', _ -> solve acc 0 0 'm' next
       | 'u', 'm' -> solve acc 0 0 'u' next
       | 'l', 'u' -> solve acc 0 0 'l' next
       | '(', 'l' -> solve acc 0 0 '(' next
       | ',', '(' -> solve acc num1 0 ',' next
       | ')', ',' -> solve (acc + (num1 * num2)) 0 0 ' ' next
       | ('0' .. '9' as c), '(' -> solve acc ((num1 * 10) + num c) num2 '(' next
       | ('0' .. '9' as c), ',' -> solve acc num1 ((num2 * 10) + num c) ',' next
       | _ -> solve acc 0 0 ' ' next)
  in
  solve 0 0 0 ' ' txt |> string_of_int
;;

let part_2 path =
  let txt = get_string path in
  let rec solve valid valid' acc num1 num2 prev rest =
    match rest with
    | "" -> acc
    | _ ->
      let next = String.drop_prefix rest 1 in
      let num n = Char.to_int n - Char.to_int '0' in
      (match rest.[0], prev with
       | 'd', _ -> solve valid ' ' acc 0 0 'd' next
       | 'o', 'd' -> solve valid ' ' acc 0 0 'o' next
       | 'n', 'o' -> solve valid ' ' acc 0 0 'n' next
       | '\'', 'n' -> solve valid ' ' acc 0 0 '\'' next
       | 't', '\'' -> solve valid 't' acc 0 0 't' next
       | '(', 'o' -> solve true 'o' acc 0 0 '(' next
       | '(', 't' -> solve false 't' acc 0 0 '(' next
       | ')', '(' ->
         (match valid' with
          | 'o' -> solve true ' ' acc 0 0 ' ' next
          | 't' -> solve false ' ' acc 0 0 ' ' next
          | _ -> failwith "invalid")
       | 'm', _ -> solve valid ' ' acc 0 0 'm' next
       | 'u', 'm' -> solve valid ' ' acc 0 0 'u' next
       | 'l', 'u' -> solve valid ' ' acc 0 0 'l' next
       | '(', 'l' -> solve valid ' ' acc 0 0 '(' next
       | ',', '(' -> solve valid ' ' acc num1 0 ',' next
       | ')', ',' ->
         if valid
         then solve valid ' ' (acc + (num1 * num2)) 0 0 ' ' next
         else solve valid ' ' acc 0 0 ' ' next
       | ('0' .. '9' as c), '(' -> solve valid ' ' acc ((num1 * 10) + num c) num2 '(' next
       | ('0' .. '9' as c), ',' -> solve valid ' ' acc num1 ((num2 * 10) + num c) ',' next
       | _ -> solve valid ' ' acc 0 0 ' ' next)
  in
  solve true ' ' 0 0 0 ' ' txt |> string_of_int
;;
