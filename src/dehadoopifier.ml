
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

let lines_from_files filename =
  let rec lines_from_files_aux in_channel acc = match (read_line in_channel) with
    | None -> List.rev acc
    | Some s -> lines_from_files_aux in_channel (s :: acc) in
  lines_from_files_aux (open_in filename) []

let package_regexp =
	Str.regexp ".*package.*"

let package_match str =
	Str.string_match package_regexp str 0

let get_package_value x =
  match x with
    | Some(v) -> v
    | None -> ""

let main =
  let dir = Sys.argv.(1) in
  let results = walk_directory_tree dir ".*\\.java" in
  List.iter (
    fun file_name -> print_endline (List.hd (List.filter (fun s -> package_match s) (lines_from_files file_name)))
  ) results

let () =
  main
