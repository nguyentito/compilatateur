let type_only = ref false
let parse_only = ref false

let print_version () =
  print_endline "MiniC : A compiler for a small subset of C\nVersion 0.42";
  exit 0

let usage_msg = "Usage : minic [options] file"
let option_list = ["-parse-only", Arg.Set parse_only, 
                                  "Stops compiling after the parsing phase";
                   "-type-only",  Arg.Set type_only, 
                                  "Stops compiling after the typing phase";
                   "--version",   Arg.Unit print_version,
                                  "Display the program name and the version number"
                  ]


(* The way this handles an error coming from an expression
   spread on multiple lines is unsatisfying *)
let signal_failure filename pos_start pos_end message =
  let line_num = pos_start.Lexing.pos_lnum
  and start_col = pos_start.Lexing.pos_cnum - pos_start.Lexing.pos_bol in
  let end_col = if pos_end.Lexing.pos_lnum = line_num
                then pos_end.Lexing.pos_cnum - pos_end.Lexing.pos_bol
                else start_col
  in
  Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n%s\n"
                 filename line_num start_col end_col message;
  exit 1

let main_exec filename =
  try
    let file = open_in filename in 
    let buf = Lexing.from_channel file in
    let syntax_error msg = signal_failure filename
                                          (Lexing.lexeme_start_p buf)
                                          (Lexing.lexeme_end_p buf)
                                          msg
    in
    try
      let program_ast = Parser.parse_source_file Lexer.get_token buf in 
      if not !parse_only then begin
        Typing.typecheck_program program_ast;
        if not !type_only then begin
          () (* production de code à venir *)
        end
      end;
    with
      | Failure s -> syntax_error s (* en supposant que l'erreur vient du lexer... *)
      | Lexer.UnterminatedComment -> syntax_error "Unterminated comment"
      | Parser.Error -> syntax_error "Syntax error"
      | Typing.Error (err, (loc_start, loc_end)) ->
          signal_failure filename loc_start loc_end (Typing.error_message err)
      | Typing.NoMainFunction -> 
          Printf.eprintf "File \"%s\", line 1, characters 0-0 :\nno main function\n" filename
      | Typing.InvalidMainFunction ->
          Printf.eprintf "File \"%s\", line 1, characters 0-0 :\ninvalid main function\n" filename

  with 
    | Sys_error _  -> Printf.eprintf "%s : file not found" filename; exit 2

let main () =
  if Array.length Sys.argv = 1
  then Arg.usage option_list usage_msg
  else Arg.parse option_list main_exec usage_msg

let _ = main ()
