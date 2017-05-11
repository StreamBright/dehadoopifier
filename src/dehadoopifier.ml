
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
          match (stat f).st_kind with
            | S_REG -> (dirs, f::files)  (* Regular file *)
            | S_DIR -> (f::dirs, files)  (* Directory *)
            | _ -> (dirs, files)
          ) ([],[]) contents
      in
      let matched = List.filter (select) files in
      walk (matched @ acc) (dirs @ tail)
  in
  walk [] [dir]

let get_first_line file_name =
 let ic = open_in file_name in
  try
    let line = input_line ic in
      flush stdout;
      close_in ic;
      Some(line);
  with e -> None

let line_stream_of_channel channel =
  Stream.from
    (fun _ -> try Some (input_line channel) with End_of_file -> None)

let get_package file_name =
  let in_channel = open_in file_name in
  try
    Stream.iter (fun line -> print_endline line) (line_stream_of_channel in_channel);
    close_in in_channel
    with e ->
    close_in in_channel;
  raise e

let stream_filter p stream =
    let rec next i =
      try
        let value = Stream.next stream in
        if p value then Some value else next i
      with Stream.Failure -> None in
    Stream.from next



let get_package_value x =
  match x with
    | Some(v) -> v
    | None -> ""

let main =
  let dir = Sys.argv.(1) in
  let results = walk_directory_tree dir ".*\\.java" in
  List.iter (fun s -> (print_endline (get_package_value (get_package s)))) results

let () =
  main
