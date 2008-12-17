type binop =
    Add | Sub | Mult | Div | Modulo
  | Or | And | Bw_or | Bw_and | Bw_xor | Lshift | Rshift
  | Equal | Neq | Less | Leq | Greater | Geq

type assignop =
    Assign | Add_assign | Sub_assign | Mult_assign | Div_assign | Modulo_assign
  | Bw_or_assign | Bw_and_assign | Bw_xor_assign | Lshift_assign | Rshift_assign

type unop =
    Not | Bw_not | Plus | Minus

type incop =
    Pre_inc | Post_inc | Pre_dec | Post_dec

type expr =
    Literal of int
  | Id of string
  | Unop of unop * expr
  | Incop of incop * string
  | Binop of expr * binop * expr
  | Assignop of string * assignop * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Break
  | Continue
  | Try_catch of stmt * string * stmt
  | Throw of expr

type func_decl = {
  _fname : string;
  _formals : string list;
  _body : stmt list;
}

type program = func_decl list

type func_decl_detail = {
  fname : string;
  formals : string list;
  locals : string list;
  body : stmt list;
}

type program_detail = func_decl_detail list
