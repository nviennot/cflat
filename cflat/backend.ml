open Ast
open Printf

type context_t = {
  label_count : int ref;
  break_label : string option;
  continue_label : string option;
  return_label : string option;
}

let get_new_label context =
  let l = !(context.label_count) in
  context.label_count := l + 1;
  ".L" ^ (string_of_int l)

let get = function
    Some(x) -> x
  | None -> ""

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

let rec eval_expr_to_eax fdecl = function
    Literal(l) -> sprintf "mov eax, %d\n" l
  | Id(s) -> sprintf "mov eax, [ebp+%d]\n" (id_to_offset fdecl s)
  | Unop(o, e) ->
      eval_expr_to_eax fdecl e ^
      (match o with
        Not      -> "test eax, eax\n" ^
                    "mov eax, 0\n" ^
                    "setz al\n"
      | Bw_not   -> "not eax\n"
      | Plus     -> ""
      | Minus    -> "neg eax\n"
      | Pre_inc  -> "NOT_IMPL\n"
      | Post_inc -> "NOT_IMPL\n"
      | Pre_dec  -> "NOT_IMPL\n"
      | Post_dec -> "NOT_IMPL\n")

  | Binop(e1, o, e2) ->
      eval_expr_to_eax fdecl e1 ^
      "push eax\n" ^
      eval_expr_to_eax fdecl e2 ^
      "pop ecx\n" ^
      "xchg eax, ecx\n" ^
      (* eax contains e1, ecx contains e2 *)

      (match o with
        Equal | Neq | Less | Leq | Greater | Geq ->
         "cmp eax, ecx\n" ^
         "mov eax, 0\n"
      | _ -> "" ) ^

      (match o with
        Add     -> "add   eax, ecx\n"
      | Sub     -> "sub   eax, ecx\n"
      | Mult    -> "imul  eax, ecx\n"
      | Div     -> "cdq\n" ^
                   "idiv  ecx\n"
      | Modulo  -> "cdq\n" ^
                   "idiv  ecx\n" ^
                   "mov   eax, edx\n"

      | Or      -> "or    eax, ecx\n" ^
                   "mov   eax, 0\n" ^
                   "setnz al\n"
      | And     -> "test  eax, eax\n" ^
                   "mov   eax, 0\n" ^
                   "setnz al\n" ^
                   "test  ecx, ecx\n" ^
                   "mov   ecx, 0\n" ^
                   "setnz cl\n" ^
                   "and   eax, ecx\n"

      | Bw_or   -> "or    eax, ecx\n"
      | Bw_and  -> "and   eax, ecx\n"
      | Bw_xor  -> "xor   eax, ecx\n"
      | Lshift  -> "sal   eax, cl\n"
      | Rshift  -> "sar   eax, cl\n"

      | Equal   -> "sete  al\n"
      | Neq     -> "setne al\n"
      | Less    -> "setl  al\n"
      | Leq     -> "setle al\n"
      | Greater -> "setg  al\n"
      | Geq     -> "setge al\n")

  | Assignop(v, o, e) ->
      let binop = (match o with
          Add_assign -> Add | Sub_assign -> Sub | Mult_assign -> Mult
        | Div_assign -> Div | Modulo_assign -> Modulo | Bw_or_assign -> Bw_or
        | Bw_and_assign -> Bw_and | Bw_xor_assign -> Bw_xor
        | Lshift_assign -> Lshift | Rshift_assign -> Rshift) in
        eval_expr_to_eax fdecl (Assign(v, Binop(Id(v), binop, e)))

  | Assign(v, e) ->
      eval_expr_to_eax fdecl e ^
      sprintf "mov [ebp+%d], eax\n" (id_to_offset fdecl v)

  | Call(f, el) ->
      ( String.concat ""
      (let prepare_arg = fun e -> eval_expr_to_eax fdecl e ^ "push eax\n" in
      (List.map prepare_arg (List.rev el))) ) ^
      sprintf "call %s\n" f ^
      sprintf "add esp, %d\n" (4 * (List.length el))
  | Noexpr -> ""

let rec string_of_stmt context fdecl = function
    Block(stmts) ->
      String.concat "" (List.map (string_of_stmt context fdecl) stmts)
  | Expr(expr) -> eval_expr_to_eax fdecl expr
  | Return(expr) ->
      eval_expr_to_eax fdecl expr ^
      "jmp " ^ (get context.return_label) ^ "\n"
  | If(e, s1, s2) ->
      let else_label    = get_new_label context
      and exit_if_label = get_new_label context in
      String.concat "\n" [
      eval_expr_to_eax fdecl e;
      "test eax, eax";
      "jz " ^ else_label;
      (string_of_stmt context fdecl s1);
      "jmp " ^ exit_if_label;
      else_label ^ ":";
      (string_of_stmt context fdecl s2);
      exit_if_label ^ ":"] ^ "\n"

  | For(e1, e2, e3, s) ->
      let loop_begin_label = get_new_label context
      and loop_label       = get_new_label context
      and loop_exit_label  = get_new_label context in
      let context' = { context with continue_label = Some loop_label;
                       break_label = Some loop_exit_label } in
      eval_expr_to_eax fdecl e1 ^
      "jmp " ^ loop_begin_label ^ "\n" ^
      loop_label ^ ":\n" ^
      eval_expr_to_eax fdecl e3 ^
      loop_begin_label ^ ":\n" ^
      (match e2 with
          Noexpr -> ""
        | _ -> eval_expr_to_eax fdecl e2 ^
               "test eax, eax\n" ^
               "jz " ^ loop_exit_label ^ "\n") ^
      string_of_stmt context' fdecl s ^
      "jmp " ^ loop_label ^ "\n" ^
      loop_exit_label ^ ":\n"

  | While(e, s) -> string_of_stmt context fdecl (For(Noexpr, e, Noexpr, s))
  | Break -> "jmp " ^ get context.break_label ^ "\n"
  | Continue -> "jmp " ^ get context.continue_label ^ "\n"
  | Try_catch(s1, e, s2) ->
      let catch_label = get_new_label context
      and exit_label  = get_new_label context in
      "push [exception_ptr]\n" ^
      "mov  [exception_ptr], esp\n" ^
      "push catch_label\n" ^
      string_of_stmt context fdecl s1 ^
      "jmp " ^ exit_label ^ "\n" ^
      catch_label ^ ":\n" ^
      string_of_stmt context fdecl s2 ^
      exit_label ^ ":\n"

let string_of_vdecl id = "int " ^ id ^ ";\n"

let string_of_fdecl context fdecl =
  let context' = { context with return_label = Some (get_new_label context) } in
  ".globl " ^ fdecl.fname ^ "\n" ^
  ".type   " ^ fdecl.fname ^ ", @function\n" ^
  fdecl.fname ^ ":\n" ^
  "push ebp\n" ^
  "mov  ebp, esp\n" ^
  "sub  esp, " ^ (string_of_int (4 * (List.length fdecl.locals))) ^ "\n" ^
  "push ecx\n" ^
  "push edx\n" ^
  String.concat "" (List.map (string_of_stmt context' fdecl) fdecl.body) ^
  get context'.return_label ^ ":\n" ^
  "pop  edx\n" ^
  "pop  ecx\n" ^
  "mov  esp, ebp\n" ^
  "pop  ebp\n" ^
  "ret\n"

let generate_asm (vars, funcs) =
  let context = { label_count = ref 0;
                  continue_label = None;
                  break_label = None;
                  return_label = None} in
  ".intel_syntax\n        .text\n        .intel_syntax noprefix\n" ^
  String.concat "\n" (List.map (string_of_fdecl context) funcs) ^
  ".ident  \"C Flat compiler 0.1\"\n        .section        .note.GNU-stack,\"\",@progbits\n"
