(* Walks a directory tree recursively starting in dir and gets you the files matching the pattern *)
let walk_directory_tree dir pattern =
  let open Unix in
  let re = Str.regexp pattern in
  let select str = Str.string_match re str 0 in
  let rec walk acc dirs = match dirs
    with
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

(* Reads a line from a channel and returns Some(line) or None if file end has reached *)
let read_line i =
  try
    Some (input_line i)
  with End_of_file ->
    None

(* Get lines from a file, closes file after read it *)
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

(* Package line regexp (what could possibly go wrong? :) *)
let package_regexp =
  Str.regexp ".*package.*"

(* Same for import as above, needs to be better*)
let import_regexp =
  Str.regexp ".*import\\ [a-zA-Z]+\\."
(* Finding regexp in a string starting from the 0th position *)
let find_matches regxp str =
  Str.string_match regxp str 0

let replace input output =
  Str.global_replace (Str.regexp_string input) output

let remove_package package =
  replace "package " "" package

let remove_import import =
  replace "import " "" import

let remove_semi_colon s =
  replace ";" "" s

let process_package package =
  let s = remove_package package in
  remove_semi_colon s

let process_import import =
  let s = remove_import import in
  remove_semi_colon s

let get_package file_name =
  process_package
  (List.hd
    (List.filter (
      fun s -> find_matches package_regexp s
    ) (lines_from_file file_name)))

let get_imports file_name =
  (List.map process_import (List.filter (fun s -> find_matches import_regexp s) (lines_from_file file_name)))

let get_class file_name =
  (List.hd (List.rev (List.tl (Str.split (Str.regexp ("/")) file_name))))

(*
  Getting 3 element tuples with (package name, file, imports)
*)
let get_triplets files =
  List.map (
    fun file_name -> (
      get_package file_name,
      get_class file_name,
      get_imports file_name
    )
  ) files

let process_imports imports =
  let clean = List.map process_import imports in
  String.concat " ; "  clean

let pp_triplets (package, file_name, imports) =
  Printf.printf "(%s, %s, [%s])\n" package file_name (process_imports imports)

let split_package package =
  Str.split (Str.regexp "\\.") package

let package_to_path package =
  String.concat "/" (split_package package)

let paths =
  let ht = Hashtbl.create 10 in
  Hashtbl.add ht "org_apache_hadoop_hive_ql" "../hive/ql/src/java";
  ht

let ht_lookup ht key =
  try Some (Hashtbl.find ht key)
  with Not_found -> None

let search_package_path full_path =
  let rec sp path_rev =
  let lookup = ht_lookup paths (String.concat "_" (List.rev path_rev)) in
  match path_rev, lookup
    with
      | head::tail, None -> sp tail
      | _, Some found -> Some found
      | head, None -> None in
  sp (List.rev full_path)

let package_from_import import =
  let full_path_as_list = List.rev (List.tl (List.tl (List.rev (Str.split (Str.regexp "\\.") import)))) in
  let package_path = search_package_path full_path_as_list in
  package_path
(*

results:

../reactively/server/java/jflow/src/main/java/com/streambright/jflow/types/WebPageView.java
../reactively/server/java/jflow/src/main/java/com/streambright/jflow/types/WebPageViewRow.java
../reactively/server/java/jflow/src/main/java/com/streambright/jflow/serde/out/OrcSerializer.java
../reactively/server/java/jflow/src/main/java/com/streambright/jflow/serde/out/ParquetSerializer.java
../reactively/server/java/jflow/src/main/java/com/streambright/jflow/serde/out/Serializer.java
../reactively/server/java/jflow/src/main/java/com/streambright/jflow/serde/in/AvroDeserializer.java
../reactively/server/java/jflow/src/main/java/com/streambright/jflow/serde/in/Deserializer.java
../reactively/server/java/jflow/src/main/java/com/streambright/jflow/io/EventProcessor.java
../reactively/server/java/jflow/src/main/java/com/streambright/jflow/io/EventProcessorJob.java
../reactively/server/java/jflow/src/main/java/com/streambright/jflow/io/S3Uploader.java
../reactively/server/java/jflow/src/main/java/com/streambright/jflow/io/S3UploaderJob.java
../reactively/server/java/jflow/src/main/java/com/streambright/jflow/http/handlers/ConfigHandler.java
../reactively/server/java/jflow/src/main/java/com/streambright/jflow/http/handlers/EchoHandler.java
../reactively/server/java/jflow/src/main/java/com/streambright/jflow/http/handlers/EventHandler.java
../reactively/server/java/jflow/src/main/java/com/streambright/jflow/http/handlers/LoginHandler.java
../reactively/server/java/jflow/src/main/java/com/streambright/jflow/http/HttpServer.java
../reactively/server/java/jflow/src/main/java/com/streambright/jflow/config/Defaults.java
../reactively/server/java/jflow/src/main/java/com/streambright/jflow/aaa/Auth.java
../reactively/server/java/jflow/src/main/java/com/streambright/jflow/App.java


*)
let main =
  let dir = Sys.argv.(1) in
  let results = walk_directory_tree dir ".*\\.java" in
  print_endline "Results :: ";
  List.iter print_endline results;
  print_endline "Triplets  :: ";
  List.iter pp_triplets (get_triplets results)

let () =
  main
