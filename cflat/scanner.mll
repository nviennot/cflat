{ open Parser }

let newline	= '\n' | "\r\n"
let whitespace  = [' ' '\t'] | newline
let digit	= ['0'-'9']
let integer	= digit+
let alpha	= ['_' 'a'-'z' 'A'-'Z']
let alphanum	= alpha | digit
let identifier	= alpha alphanum*

rule token = parse
  whitespace            { token lexbuf }
| "//" _* newline	{ token lexbuf }
| "/*"			{ comment 0 lexbuf }

(* arithmetic operators *)
| "++"			{ INC }
| "--"			{ DEC }
| "-="			{ MINUS_ASSIGN }
| "+="			{ PLUS_ASSIGN }
| "*="			{ TIMES_ASSIGN }
| "/="			{ DIVIDE_ASSIGN }
| "%="			{ MODULO_ASSIGN }
| '-'			{ MINUS }
| '+'			{ PLUS }
| '*'			{ TIMES }
| '/'			{ DIVIDE }
| '%'			{ MODULO }

(* bitwise operators *)
| "<<="			{ LSHIFT_ASSIGN }
| ">>="			{ RSHIFT_ASSIGN }
| "&="			{ BW_AND_ASSIGN }
| "|="			{ BW_OR_ASSIGN }
| "^="			{ BW_XOR_ASSIGN }
| "<<"			{ LSHIFT }
| ">>"			{ RSHIFT }
| "~"			{ BW_NOT }
| "&[^&]"		{ BW_AND }
| "|[^|]"		{ BW_OR }
| "^"			{ BW_XOR }

(* logic operators *)
| "<="			{ LEQ }
| ">="			{ GEQ }
| "!="			{ NEQ }
| "=="			{ EQ }
| "!"			{ NOT }
| "&&"			{ AND }
| "||"			{ OR }
| '<'			{ LT }
| '>'			{ GT }

(* punctuation *)
| '='			{ ASSIGN }
| '('			{ LPAREN }
| ')'			{ RPAREN }
| '{'			{ LBRACE }
| '}'			{ RBRACE }
| ';'			{ SEMI }
| ','			{ COMMA }

(* keywords *)
| "for"			{ FOR }
| "while"		{ WHILE }
| "if"			{ IF }
| "else"		{ ELSE }
| "goto"		{ GOTO }
| "return"		{ RETURN }
| "int"			{ INT }
| "break"		{ BREAK }
| "continue"		{ CONTINUE }
| "try"			{ TRY }
| "catch"		{ CATCH }
| integer as lit 	{ LITERAL(int_of_string lit) }
| identifier as id	{ ID(id) }

| eof			{ EOF }
| _ as char		{ raise (Failure("illegal character " ^ Char.escaped char)) }

and comment level = parse
  "*/" { if level = 0 then token lexbuf
         else comment (level-1) lexbuf }
| "/*" { comment (level+1) lexbuf }
| _    { comment level lexbuf }
