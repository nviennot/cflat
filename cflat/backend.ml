open Ast
open Printf

type context_t = {
  label_count        : int ref;
  break_label        : string option;
  continue_label     : string option;
  return_label       : string option;
  function_try_level : int;
  loop_try_level     : int;
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

(*
an exception looks like this:
    struct exception {
      struct exception *next;
      void *catch_address;
      int  old_ebp;
      int  old_esp;
    };
*)

let exception_context_size = 16

let stack_exception catch_label =
  sprintf "lea eax, %s\n" catch_label ^
          "push esp\n" ^
          "push ebp\n" ^
          "push eax\n" ^
          "push dword ptr [__exception_ptr]\n" ^
          "mov  [__exception_ptr], esp\n"

let unstack_exception n =
  sprintf "add esp, %d\n" (exception_context_size * n)

let rec unwind_exception = function
    0 -> ""
  | n -> "mov eax, [__exception_ptr]\n" ^
         "mov eax, [eax]\n" ^
         "mov [__exception_ptr], eax\n" ^
         unwind_exception (n-1)

let rec eval_expr_to_eax fdecl = function
    Literal(l) ->
      sprintf "mov eax, %d\n" l

  | Id(s) ->
      sprintf "mov eax, [ebp+%d]\n" (id_to_offset fdecl s)

  | Unop(o, e) ->
      (match (o, e) with
        (Pre_inc, Id(s)) ->
          sprintf "inc dword ptr [ebp+%d]\n" (id_to_offset fdecl s)
      | (Pre_dec, Id(s)) ->
          sprintf "dec dword ptr [ebp+%d]\n" (id_to_offset fdecl s)
      | _ -> "") ^
      eval_expr_to_eax fdecl e ^
      (match o with
        Not      -> "test  eax, eax\n" ^
                    "setz  al\n" ^
                    "movzx eax, al\n"
      | Bw_not   -> "not   eax\n"
      | Plus     -> ""
      | Minus    -> "neg   eax\n"
      | Pre_inc | Post_inc | Pre_dec | Post_dec -> "") ^
      (match (o, e) with
        (Post_inc, Id(s)) ->
          sprintf "inc dword ptr [ebp+%d]\n" (id_to_offset fdecl s)
      | (Post_dec, Id(s)) ->
          sprintf "dec dword ptr [ebp+%d]\n" (id_to_offset fdecl s)
      | _ -> "")

  | Binop(e1, o, e2) ->
      eval_expr_to_eax fdecl e1 ^
      "push eax\n" ^
      eval_expr_to_eax fdecl e2 ^
      "pop ecx\n" ^
      "xchg eax, ecx\n" ^
      (* eax contains e1, ecx contains e2 *)

      (match o with
        Equal | Neq | Less | Leq | Greater | Geq ->
          "cmp eax, ecx\n"
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
                   "setnz al\n"
      | And     -> "test  eax, eax\n" ^
                   "setnz al\n" ^
                   "test  ecx, ecx\n" ^
                   "setnz cl\n" ^
                   "and   al, cl\n"
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
      | Geq     -> "setge al\n") ^

      (match o with
        Or | And | Equal | Neq | Less | Leq | Greater | Geq ->
          "movzx eax, al\n"
      | _ -> "" )

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
      (let prepare_arg = fun e ->
                            eval_expr_to_eax fdecl e ^
                            "push eax\n" in
      (List.map prepare_arg el))) ^
      sprintf "push %d\n" (List.length el) ^
              "call __reverse_args\n" ^
              "add  esp, 4\n" ^
      sprintf "call %s\n" f ^
      sprintf "add  esp, %d\n" (4 * (List.length el))
  | Noexpr -> ""

let rec string_of_stmt context fdecl = function
    Block(stmts) ->
      String.concat "" (List.map (string_of_stmt context fdecl) stmts)

  | Expr(expr) -> eval_expr_to_eax fdecl expr

  | Return(expr) ->
      unwind_exception  context.function_try_level ^
      unstack_exception context.function_try_level ^
      eval_expr_to_eax fdecl expr ^
      sprintf "jmp %s\n" (get context.return_label)

  | If(e, s1, s2) ->
      let else_label    = get_new_label context
      and exit_if_label = get_new_label context in
      eval_expr_to_eax fdecl e ^
              "test eax, eax\n" ^
      sprintf "jz %s\n" else_label ^
      string_of_stmt context fdecl s1 ^
      sprintf "jmp %s\n" exit_if_label ^
      sprintf "%s:\n" else_label ^
      string_of_stmt context fdecl s2 ^
      sprintf "%s:\n" exit_if_label

  | For(e1, e2, e3, s) ->
      let loop_begin_label = get_new_label context
      and loop_label       = get_new_label context
      and loop_exit_label  = get_new_label context in
      let context' = { context with continue_label = Some loop_label;
                                    break_label    = Some loop_exit_label;
                                    loop_try_level = 0 } in
      eval_expr_to_eax fdecl e1 ^
      sprintf "jmp %s\n" loop_begin_label ^
      sprintf "%s:\n" loop_label ^
      eval_expr_to_eax fdecl e3 ^
      sprintf "%s:\n" loop_begin_label ^
      (match e2 with
         Noexpr -> ""
       | _ -> eval_expr_to_eax fdecl e2 ^
                      "test eax, eax\n" ^
              sprintf "jz %s\n" loop_exit_label) ^
      string_of_stmt context' fdecl s ^
      sprintf "jmp %s\n" loop_label ^
      sprintf "%s:\n" loop_exit_label

  | While(e, s) -> string_of_stmt context fdecl (For(Noexpr, e, Noexpr, s))

  | Break ->
      unwind_exception  context.loop_try_level ^
      unstack_exception context.loop_try_level ^
      sprintf "jmp %s\n" (get context.break_label)

  | Continue ->
      unwind_exception  context.loop_try_level ^
      unstack_exception context.loop_try_level ^
      sprintf "jmp %s\n" (get context.continue_label)

  | Try_catch(s1, e, s2) ->
      let catch_label      = get_new_label context
      and exit_label       = get_new_label context in
      let context' = { context with
                         function_try_level = context.function_try_level + 1;
                         loop_try_level     = context.loop_try_level + 1} in
      stack_exception catch_label ^
      string_of_stmt context' fdecl s1 ^
      unwind_exception 1 ^
      unstack_exception 1 ^
      sprintf "jmp %s\n" exit_label ^
      sprintf "%s:\n" catch_label ^
      (match e with
        Noexpr -> ""
      | Id(v)  -> sprintf "mov [ebp+%d], eax\n" (id_to_offset fdecl v)
      | _      -> "ERROR\n") ^
      string_of_stmt context fdecl s2 ^
      sprintf "%s:\n" exit_label

  | Throw(e) ->
      "push [__exception_ptr]\n" ^
      unwind_exception 1 ^
      eval_expr_to_eax fdecl e ^
      "pop  ecx\n" ^
      "mov  esp, [ecx+12]\n" ^ (* exception is unstacked *)
      "mov  ebp, [ecx+8]\n" ^
      "jmp  [ecx+4]\n"

let string_of_vdecl id = "int " ^ id ^ ";\n"

let string_of_fdecl context fdecl =
  let context' = { context with return_label = Some (get_new_label context) } in
  sprintf ".globl %s\n" fdecl.fname ^
  sprintf ".type %s, @function\n" fdecl.fname ^
  sprintf "%s:\n" fdecl.fname ^
          "push ebp\n" ^
          "mov  ebp, esp\n" ^
  sprintf "sub  esp, %d\n" (4 * (List.length fdecl.locals)) ^
          "push ecx\n" ^
          "push edx\n" ^
  String.concat "" (List.map (string_of_stmt context' fdecl) fdecl.body) ^
  sprintf "%s:\n" (get context'.return_label) ^
          "pop  edx\n" ^
          "pop  ecx\n" ^
          "mov  esp, ebp\n" ^
          "pop  ebp\n" ^
          "ret\n"

let generate_asm (vars, funcs) =
  let context = { label_count        = ref 0;
                  continue_label     = None;
                  break_label        = None;
                  return_label       = None;
                  function_try_level = 0;
                  loop_try_level     = 0 } in
  ".intel_syntax noprefix\n" ^
  ".text\n" ^
  String.concat "" (List.map (string_of_fdecl context) funcs) ^
  ".ident \"C Flat compiler 0.1\"\n"
