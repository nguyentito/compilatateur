let type_only = ref false
let parse_only = ref false

let option_list = ["-parse-only", Arg.Set parse_only, 
                                  "Stops compiling after the parsing phase";
                   "-type-only",  Arg.Set type_only, 
                                  "Stops compiling after the typing phase";
                  ]


(* Ne marche pas bien pour une bloc de code erroné sur plusieurs lignes *)
let signal_failure filename pos_start pos_end message =
  Printf.eprintf "File \"%s\", line %d, characters %d-%d :\n%s\n"
                 filename pos_start.Lexing.pos_lnum
                 (pos_start.Lexing.pos_cnum - pos_start.Lexing.pos_bol)
                 (pos_end.Lexing.pos_cnum - pos_end.Lexing.pos_bol)
                 message;
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
      let file_parsed = Parser.parse_source_file Lexer.get_token buf in 
      if !parse_only then () 
      else
        begin
          Typing.typecheck_program file_parsed;
          if !type_only then ()
          else
            (); (* en attendant d'avoir fait la production de code*)
        end;
    with
      | Failure s -> syntax_error s
      | Parser.Error -> syntax_error "Syntax error"
      | Typing.Error (err, (loc_start, loc_end)) ->
          signal_failure filename loc_start loc_end "foobar"

  with 
    | Sys_error _  -> Printf.eprintf "%s : file not found" filename; exit 1

let main () = Arg.parse option_list main_exec ""

let _ = main ()
