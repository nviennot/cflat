%{ open Ast %}

/*(*
microc original shit

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE INT BREAK CONTINUE
%token <int> LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE

%left ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
*)*/

%token INC DEC MINUS_ASSIGN PLUS_ASSIGN TIMES_ASSIGN DIVIDE_ASSIGN MODULO_ASSIGN
%token MINUS PLUS TIMES DIVIDE MODULO
%token LSHIFT_ASSIGN RSHIFT_ASSIGN BW_AND_ASSIGN BW_OR_ASSIGN BW_XOR_ASSIGN
%token LSHIFT RSHIFT BW_NOT BW_AND BW_OR BW_XOR
%token LEQ GEQ NEQ EQ NOT AND OR LT GT
%token ASSIGN LPAREN RPAREN LBRACE RBRACE SEMI COMMA
%token FOR WHILE IF ELSE GOTO RETURN INT BREAK CONTINUE TRY CATCH
%token <int> LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE

%left BW_AND_ASSIGN BW_XOR_ASSIGN BW_OR_ASSIGN
%left LEFT_SHIFT_ASSIGN RIGHT_SHIFT_ASSIGN
%left TIMES_ASSIGN DIVIDE_ASSIGN MODULO_ASSIGN
%left PLUS_ASSIGN MINUS_ASSIGN
%left ASSIGN
%left OR
%left AND
%left BW_AND BW_XOR BW_OR
%left EQ NEQ
%left GT GEQ
%left LT LEQ
%left LSHIFT RSHIFT
%left PLUS MINUS /*(* is it ok ?? unary/binary *)*/
%left TIMES DIVIDE MODULO
%right NOT BW_NOT
%left INC DEC

/*(*
wanted precedence
%left SEMI
%left COMMA
%left LBRACE RBRACE
%left BW_AND_ASSIGN BW_XOR_ASSIGN BW_OR_ASSIGN
%left LEFT_SHIFT_ASSIGN RIGHT_SHIFT_ASSIGN
%left TIMES_ASSIGN DIVIDE_ASSIGN MODULO_ASSIGN
%left PLUS_ASSIGN MINUS_ASSIGN
%left ASSIGN
%left OR
%left AND
%left BW_AND BW_XOR BW_OR
%left EQ NEQ
%left GT GEQ
%left LT LEQ
%left LSHIFT RSHIFT
(*%left PLUS MINUS (* is it ok ?? unary/binary *)*)
%left TIMES DIVIDE MODULO
%right NOT BW_NOT
%left LPAREN RPAREN
%left PLUS MINUS (* is it ok ?? unary/binary *)
%left INC DEC
%left FOR WHILE IF ELSE GOTO RETURN
*)*/


%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }
 | program fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { fname = $1;
	 formals = $3;
	 locals = List.rev $6;
	 body = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   INT ID SEMI { $2 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | BREAK SEMI { Break }
  | CONTINUE SEMI { Continue }
  | TRY stmt CATCH LPAREN expr RPAREN stmt { Try_catch($2, $5, $7) }
  | TRY stmt CATCH stmt { Try_catch($2, Noexpr, $4) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1) }
  | ID               { Id($1) }

  | expr OR       expr { Binop($1, Or,      $3) }
  | expr AND      expr { Binop($1, And,     $3) }
  | expr BW_OR    expr { Binop($1, Bw_or,   $3) }
  | expr BW_AND   expr { Binop($1, Bw_and,  $3) }
  | expr BW_XOR   expr { Binop($1, Bw_xor,  $3) }
  | expr LSHIFT   expr { Binop($1, Lshift,  $3) }
  | expr RSHIFT   expr { Binop($1, Rshift,  $3) }
  | expr PLUS     expr { Binop($1, Add,     $3) }
  | expr MINUS    expr { Binop($1, Sub,     $3) }
  | expr TIMES    expr { Binop($1, Mult,    $3) }
  | expr DIVIDE   expr { Binop($1, Div,     $3) }
  | expr MODULO   expr { Binop($1, Modulo,  $3) }
  | expr EQ       expr { Binop($1, Equal,   $3) }
  | expr NEQ      expr { Binop($1, Neq,     $3) }
  | expr LT       expr { Binop($1, Less,    $3) }
  | expr LEQ      expr { Binop($1, Leq,     $3) }
  | expr GT       expr { Binop($1, Greater, $3) }
  | expr GEQ      expr { Binop($1, Geq,     $3) }

  | NOT    expr { Unop(Not,      $2) }
  | BW_NOT expr { Unop(Bw_not,   $2) }
  | PLUS   expr { Unop(Plus,     $2) }
  | MINUS  expr { Unop(Minus,    $2) }
  | INC    expr { Unop(Pre_inc,  $2) }
  | DEC    expr { Unop(Pre_dec,  $2) }
  | expr   INC  { Unop(Post_inc, $1) }
  | expr   DEC  { Unop(Post_dec, $1) }
 
  | ID BW_AND_ASSIGN expr { Assignop($1, Bw_and_assign, $3) }
  | ID BW_OR_ASSIGN  expr { Assignop($1, Bw_or_assign,  $3) }
  | ID BW_XOR_ASSIGN expr { Assignop($1, Bw_xor_assign, $3) }
  | ID LSHIFT_ASSIGN expr { Assignop($1, Lshift_assign, $3) }
  | ID RSHIFT_ASSIGN expr { Assignop($1, Rshift_assign, $3) }
  | ID TIMES_ASSIGN  expr { Assignop($1, Mult_assign,   $3) }
  | ID DIVIDE_ASSIGN expr { Assignop($1, Div_assign,    $3) }
  | ID MODULO_ASSIGN expr { Assignop($1, Modulo_assign, $3) }
  | ID PLUS_ASSIGN   expr { Assignop($1, Add_assign,    $3) }
  | ID MINUS_ASSIGN  expr { Assignop($1, Sub_assign,    $3) }
  | ID ASSIGN expr        { Assign($1, $3) }

  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
