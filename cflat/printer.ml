open Ast

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Id(s) -> s
  | Unop(o, e) ->
      (match o with
        Not -> "!" | Bw_not -> "~" | Plus -> "+" | Minus -> "-") ^
      string_of_expr e
  | Incop(o, v) ->
    (match o with Pre_inc -> "++" | Pre_dec -> "--" | _ -> "") ^
    v ^
    (match o with Post_inc -> "++" | Post_dec -> "--"  | _ -> "")
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
        Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/" | Modulo -> "%"
      | Or -> "||" | And -> "&&" | Bw_or -> "|" | Bw_and -> "&" | Bw_xor -> "^"
      | Lshift -> "<<" | Rshift -> ">>"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
      string_of_expr e2
  | Assignop(v, o, e) ->
      v ^ " " ^
      (match o with
          Add_assign -> "+" | Sub_assign -> "-" | Mult_assign -> "*"
        | Div_assign -> "/" | Modulo_assign -> "%" | Bw_or_assign -> "|"
        | Bw_and_assign -> "&" | Bw_xor_assign -> "^"
        | Lshift_assign -> "<<" | Rshift_assign -> ">>") ^
      "= " ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Break -> "break"
  | Continue -> "continue"
  | Try_catch(s1, "", s2) ->
      "try\n" ^ string_of_stmt s1 ^
      "catch\n" ^ string_of_stmt s2
  | Try_catch(s1, s, s2) ->
      "try\n" ^ string_of_stmt s1 ^
      "catch (" ^ s ^ ")\n" ^ string_of_stmt s2
  | Throw(e) -> "throw " ^ string_of_expr e ^ ";\n"

let string_of_vdecl id = "int " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
