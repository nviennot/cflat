let whitespace = [' ' '\t']
let newline = '\n' | "\r\n"
let digit = ['0'-'9']
let integer = digit+
let alpha = ['_' 'a'-'z' 'A'-'Z']
let alphanum = [alpha digit]
let identifier = alpha alphanum*

rule token = parse
  whitespace		{ token lexbuf }
| "//" _* newline	{ token lexbuf }
(* /* */ comments ?*)


(* arithmetic operators *)
| "++"			{ INCREMENT }
| "--"			{ DECREMENT }
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
| "<<="			{ LEFT_SHIFT_ASSIGN }
| ">>="			{ RIGHT_SHIFT_ASSIGN }
| "&="			{ BITWISE_AND_ASSIGN }
| "|="			{ BITWISE_OR_ASSIGN }
| "^="			{ BITWISE_XOR_ASSIGN }
| "<<"			{ LEFT_SHIFT }
| ">>"			{ RIGHT_SHIFT }
| "~"			{ BITWISE_NOT }
| "&[^&]"		{ BITWISE_AND }
| "|[^|]"		{ BITWISE_OR }
| "^"			{ BITWISE_XOR }

(* comparison operators *)
| "<="			{ LESS_EQUAL }
| ">="			{ GREATER_EQUAL }
| "!="			{ NOT_EQUAL }
| "=="			{ EQUAL }
| "!"			{ NOT }
| "&&"			{ AND }
| "||"			{ OR }
| '<'			{ LESS }
| '>'			{ GREATER }

| '='			{ ASSIGN }

(* more operators *)

| '('			{ LEFT_PARENTHESIS }
| ')'			{ RIGHT_PARENTHESIS }

| '{'			{ LEFT_CURLY_BRACKET }
| '}'			{ RIGHT_CURLY_BRACKET }

| ';'			{ SEMI_COLON }
| ','			{ COMMA }

(* keywords *)
| "for"			{ FOR }
| "while"		{ WHILE }
| "if"			{ IF }
| "else"		{ ELSE }
| "goto"		{ GOTO }
| "return"		{ RETURN }
| integer as lit 	{ LITERAL(lit) }
| identifier as id	{ IDENTIFIER(id) }
