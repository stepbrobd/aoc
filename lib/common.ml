let panic msg =
  Printf.eprintf "error: %s\n" msg;
  exit 1
;;

let list_files_in_dir dir =
  match Sys.readdir dir with
  | files -> Array.to_list files
  | exception _ -> panic (Printf.sprintf "could not read directory: %s" dir)
;;

let load_module year day =
  let cmo = Printf.sprintf "_build/default/src/year_%s/_day%s.cmo" year day in
  if not (Sys.file_exists cmo)
  then panic (Printf.sprintf "%s not found" cmo)
  else Dynlink.loadfile cmo
;;
