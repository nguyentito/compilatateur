let parse_file filename =
  try
    let buf = Lexing.from_channel (open_in filename) in
    ignore (Parser.parse_source_file Lexer.get_token buf);
    true
  with
      Sys_error _ -> failwith "File not found."
    | Parser.Error -> false
    | Failure _ -> false

let run_tests name dir =
  print_endline name;
  let file_list = Sys.readdir ("tests/" ^ dir) in
  Array.sort compare file_list;
  let run_test file =
    if Str.string_match (Str.regexp ".*\\.c") file 0 then begin
      let path = Printf.sprintf "tests/%s/%s" dir file in
      if parse_file path
      then Printf.printf "%s : success\n" path
      else Printf.printf "%s : failure\n" path
    end
  in
  Array.iter run_test file_list;
  print_newline ()

(* Pour l'instant seul le parsing est testé
   Tous les tests à part syntax/bad devraient réussir *)
let _ =
  run_tests "Tests positifs de syntaxe" "syntax/good";
  run_tests "Tests négatifs de syntaxe" "syntax/bad";
  run_tests "Tests positifs de typage" "typing/good";
  run_tests "Tests négatifs de typage" "typing/bad";
  run_tests "Tests positifs d'exécution" "exec";
  run_tests "Tests négatifs d'exécution" "exec-fail"
