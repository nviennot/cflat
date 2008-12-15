let print = false

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let program_detail = Sast.check_program program in
  if print then
    print_string (Printer.string_of_program program_detail)
  else
    print_string (Backend.generate_asm program_detail)
