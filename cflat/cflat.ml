let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let program = Parser.program Scanner.token lexbuf in
    let program_detail = Sast.check_program program in
      print_string (Backend.generate_asm program_detail); exit 0
  with
      Failure(s)          -> prerr_endline ("Error: " ^ s); exit 1
    | Parsing.Parse_error -> prerr_endline ("Syntax error"); exit 1
