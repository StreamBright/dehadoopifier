let walk_directory_tree dir pattern =
  let open Unix in
  let re = Str.regexp pattern in
  let select str = Str.string_match re str 0 in
  let rec walk acc = function
    | [] -> (acc)
    | dir::tail ->
      let contents = Array.to_list (Sys.readdir dir) in
      let contents = List.rev_map (Filename.concat dir) contents in
      let dirs, files =
        List.fold_left (fun (dirs,files) f ->
          match (Unix.stat f).st_kind with
            | S_REG -> (dirs, f::files)  (* Regular file *)
            | S_DIR -> (f::dirs, files)  (* Directory *)
            | _ -> (dirs, files)
          ) ([],[]) contents
      in
      let matched = List.filter (select) files in
      walk (matched @ acc) (dirs @ tail)
  in
  walk [] [dir]

let read_line i =
	try
		Some (input_line i)
	with End_of_file ->
		None

let lines_from_file filename =
	let file_in = open_in filename in
	let clean_up channel_in = close_in channel_in in
  let rec lines_from_file_aux in_channel acc =
		match (read_line in_channel) with
    	| None ->
				clean_up file_in;
				List.rev acc
    	| Some s ->
					lines_from_file_aux in_channel (s :: acc) in
  lines_from_file_aux file_in []

let package_regexp =
	Str.regexp ".*package.*"

let import_regexp =
	Str.regexp ".*import.*"

let find_matches regxp str =
	Str.string_match regxp str 0

let get_packages files =
	List.map (
    fun file_name -> (file_name, (List.hd (List.filter (fun s -> find_matches package_regexp s) (lines_from_file file_name))))
  ) files

let get_imports files =
  List.map (
    fun file_name -> (file_name, (List.filter (fun s -> find_matches import_regexp s) (lines_from_file file_name)))
  ) files

let pp_str_tuple (x,y) =
  Printf.printf "(%s,%s)\n" x y

let pp_str_list_tuple (x,l) =
	Printf.printf "(%s,%s)\n" x (String.concat " ; "  l)

let main =
  let dir = Sys.argv.(1) in
  let results = walk_directory_tree dir ".*\\.java" in
  List.iter pp_str_tuple (get_packages results) ;
	List.iter pp_str_list_tuple (get_imports results)

let () =
  main
