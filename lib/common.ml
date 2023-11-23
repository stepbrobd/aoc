let file_exists path = Sys.file_exists path

let error_and_exit msg =
  Printf.eprintf "error: %s\n" msg;
  exit 1
;;

let list_files_in_dir dir =
  match Sys.readdir dir with
  | files -> Array.to_list files
  | exception _ -> error_and_exit (Printf.sprintf "could not read directory: %s" dir)
;;

let load_module year day =
  let cmo_path = Printf.sprintf "_build/default/src/%s/%s.cmo" year day in
  if not (file_exists cmo_path)
  then
    error_and_exit
      (Printf.sprintf "module %s/%s not found. Did you run 'dune build'?" year day);
  Dynlink.loadfile cmo_path
;;
