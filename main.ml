let type_only = ref false
let parse_only = ref false

let option_list = ["-parse-only", Arg.Set parse_only, 
                                  "Stops compiling after the parsing phase";
                   "-type-only",  Arg.Set type_only, 
                                  "Stops compiling after the typing phase";
                  ]


let signal_failure lexpos message =
  Printf.eprintf "File \"%s\", line %d, characters %d-%d :\n%s\n"
                 lexpos.Lexing.pos_fname lexpos.Lexing.pos_lnum
                 lexpos.Lexing.pos_bol lexpos.Lexing.pos_cnum
                 message;
  exit 2

let main_exec filename = 
  try 
    let file = open_in filename in 

    let buf = Lexing.from_channel file in
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
      | Failure s -> signal_failure (Lexing.lexeme_start_p buf) s
      | Parser.Error -> signal_failure (Lexing.lexeme_start_p buf) "Syntax error"
      | exn -> signal_failure (Lexing.lexeme_start_p buf) (Printexc.to_string exn)

  with 
    | Sys_error _  -> Printf.eprintf "%s : file not found" filename; exit 1

let main () = Arg.parse option_list main_exec ""

let _ = main ()
