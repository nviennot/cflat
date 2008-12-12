type binop =
    Add | Sub | Mult | Div | Modulo
  | Or | And | Bw_or | Bw_and | Bw_xor | Lshift | Rshift
  | Equal | Neq | Less | Leq | Greater | Geq

type assignop =
    Add_assign | Sub_assign | Mult_assign | Div_assign | Modulo_assign
  | Bw_or_assign | Bw_and_assign | Bw_xor_assign | Lshift_assign | Rshift_assign

type unop =
    Not | Bw_not | Plus | Minus | Pre_inc | Post_inc | Pre_dec | Post_dec

type expr =
    Literal of int
  | Id of string
  | Unop of unop * expr
  | Binop of expr * binop * expr
  | Assignop of string * assignop * expr
  | Assign of string * expr
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
  | Try_catch of stmt * expr * stmt

type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
  }

type program = string list * func_decl list
