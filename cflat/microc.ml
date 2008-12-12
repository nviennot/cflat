let print = false

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  if print then
    let listing = Printer.string_of_program program in  
    print_string listing
  else
    print_string (Backend.generate_asm program)
