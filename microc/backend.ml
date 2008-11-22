open Ast
open Printf


let rec index_of item n = function
     [] -> -1
   | hd::tl -> if hd = item then n else (index_of item (n+1) tl)


let id_to_offset fdecl id =
  let n = index_of id 0 fdecl.formals in
  if n >= 0 then
    4 * (n+2)
  else
    let n = index_of id 0 fdecl.locals in
    if n >= 0 then
      -4 * (n+1)
    else
      raise (Failure ("undefined identifier " ^ id))


let eval_expr_to_eax fdecl = function
     Literal(l) -> sprintf "mov eax, %d\n" l
  |  Id(s) -> sprintf "mov eax, [ebp+%d]\n" (id_to_offset fdecl s)
  | _ -> "BLAH"


let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
      string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt fdecl = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map (string_of_stmt fdecl) stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> eval_expr_to_eax fdecl expr ^ "jmp " ^ fdecl.fname ^ "_exit\n"
  | If(e, s1, s2) ->
      String.concat "\n" [
      eval_expr_to_eax fdecl e;
      "test eax, eax";
      "jz else";
      (string_of_stmt fdecl s1);
      "jmp endif";
      "else:";
      (string_of_stmt fdecl s2);
      "endif:"] ^ "\n"

  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt fdecl s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt fdecl s

let string_of_vdecl id = "int " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  ".globl " ^ fdecl.fname ^ "\n" ^
  "        .type   " ^ fdecl.fname ^ ", @function\n" ^
  fdecl.fname ^ ":\n" ^
  "        push    ebp\n" ^
  "        mov     ebp, esp\n" ^
  "        sub     esp, " ^ (string_of_int (4 * (List.length fdecl.locals))) ^ "\n" ^
  (* fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^ *)
  String.concat "" (List.map (string_of_stmt fdecl) fdecl.body) ^
  fdecl.fname ^ "_exit:\n" ^
  "        mov     esp, ebp\n" ^
  "        pop     ebp\n" ^
  "        ret\n"

let generate_asm (vars, funcs) =
  "        .intel_syntax\n        .text\n        .intel_syntax noprefix\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^
  "        .ident  \"Nicos super awesome compiler\"\n        .section        .note.GNU-stack,\"\",@progbits\n"



(*
main() {
    x = 2;
    foo(x + 3);
    return 0;
}

.globl main
        .type   main, @function
main:
        push    ebp
        mov     ebp, esp



        pop ebp
        ret
        .size   main, .-main
*)
