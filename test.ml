let test_parsing chan =
  let buf = Lexing.from_channel chan in
  ignore (Parser.parse_source_file Lexer.get_token buf)

let test_type_checking chan =
  let buf = Lexing.from_channel chan in
  Typing.typecheck_program (Parser.parse_source_file Lexer.get_token buf)

let exn_msg = function
  | Failure _ -> "Lexical error"
  | Parser.Error -> "Parse error"
  | exn -> Printexc.to_string exn

let run_tests name dir test_fun =
  print_endline name;
  let file_list = Sys.readdir ("tests/" ^ dir) in
  Array.sort compare file_list;
  let run_test filename =
    if Str.string_match (Str.regexp ".*\\.c") filename 0 then begin
      let path = Printf.sprintf "tests/%s/%s" dir filename in
      try
        test_fun (open_in path);
        Printf.printf "%s : success\n" path
      with
        | Sys_error _ -> Printf.printf "%s : file not found" path
        | exn -> Printf.printf "%s : failure (%s)\n" path (exn_msg exn)
    end
  in
  Array.iter run_test file_list;
  print_newline ()

(* Pour l'instant seul le parsing est testé
   Tous les tests à part syntax/bad devraient réussir *)
let _ =
  run_tests "Tests positifs de syntaxe" "syntax/good" test_parsing;
  run_tests "Tests négatifs de syntaxe" "syntax/bad" test_parsing;
  run_tests "Tests positifs de typage" "typing/good" test_type_checking;
  run_tests "Tests négatifs de typage" "typing/bad" test_type_checking;
  run_tests "Tests positifs d'exécution" "exec" test_type_checking;
  run_tests "Tests négatifs d'exécution" "exec-fail" test_type_checking
