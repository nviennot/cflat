let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let program_detail = Sast.check_program program in
    print_string (Backend.generate_asm program_detail)
