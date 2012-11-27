let exec_test cmd path =
  match Unix.system (cmd ^ " " ^ path) with
    | Unix.WEXITED 0 -> print_endline "Success"
    | Unix.WEXITED 1 -> print_endline "Failure"
    | _ -> print_endline "Execution terminated abnormally"

let run_tests name dir test_cmd =
  print_endline name;
  let file_list = Sys.readdir ("tests/" ^ dir) in
  Array.sort compare file_list;
  let run_test filename =
    if Str.string_match (Str.regexp ".*\\.c") filename 0 then begin
      let path = Printf.sprintf "tests/%s/%s" dir filename in
      try
        print_endline path;
        exec_test test_cmd path;
        print_newline ()
      with
        | Sys_error _ -> print_endline "File not found"
    end
  in
  Array.iter run_test file_list;
  print_newline ()

(* Pour l'instant seul le parsing/typing est testé
   Les tests d'exécution devraient tous réussir *)
let _ =
  run_tests "Tests positifs de syntaxe"  "syntax/good" "./minic -parse-only";
  run_tests "Tests négatifs de syntaxe"  "syntax/bad"  "./minic -parse-only";
  run_tests "Tests positifs de typage"   "typing/good" "./minic -type-only";
  run_tests "Tests négatifs de typage"   "typing/bad"  "./minic -type-only";
  run_tests "Tests positifs d'exécution" "exec"        "./minic";
  run_tests "Tests négatifs d'exécution" "exec-fail"   "./minic"
