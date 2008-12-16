open Ast
open Printf

type context = {
  in_loop   : bool;
  variables : string list ref;
}

(* return l1 - l2 *)
let rec diff_list l1 = function
    [] -> l1
  | hd2 :: tl2 ->
    let rec diff_hd2 = function
        [] -> []
      | hd1 :: tl1 ->
          if hd1 = hd2 then diff_hd2 tl1
          else hd1 :: diff_hd2 tl1 in
    diff_list (diff_hd2 l1) tl2

(* add v to the context.variables list if not in the list *)
let add_variable context v =
  let rec add_unique_v = function
          [] -> [v]
        | hd :: tl ->
            if hd = v then hd :: tl
            else hd :: add_unique_v tl in
  context.variables := add_unique_v !(context.variables)

let rec check_expr fdecl context = function
    Literal(_)           -> ()
  | Id(v)                -> add_variable context v
  | Unop(_, e)           -> check_expr fdecl context e
  | Incop(_, v)          -> add_variable context v
  | Binop(e1, _, e2)     -> check_expr fdecl context e1;
                            check_expr fdecl context e2
  | Assignop(v, _, e)    -> add_variable context v;
                            check_expr fdecl context e
  | Assign (v, e)        -> add_variable context v;
                            check_expr fdecl context e
  | Call(_, el)          -> List.iter (check_expr fdecl context) el
  | Noexpr               -> ()

let rec check_stmt fdecl context = function
    Block(sl)            -> List.iter (check_stmt fdecl context) sl
  | Expr(e)              -> check_expr fdecl context e
  | Return(e)            -> check_expr fdecl context e
  | If(e, s1, s2)        -> check_expr fdecl context e;
                            check_stmt fdecl context s1;
                            check_stmt fdecl context s2
  | For(e1, e2, e3, s)   ->
      let context' = { context with in_loop = true } in
                            check_expr fdecl context' e1;
                            check_expr fdecl context' e2;
                            check_expr fdecl context' e3;
                            check_stmt fdecl context' s
  | While(e, s)          -> check_stmt fdecl context (For(Noexpr, e, Noexpr, s))
  | Break                -> if not context.in_loop then
                              raise (Failure ("break keyword used outside a loop"))
  | Continue             -> if not context.in_loop then
                              raise (Failure ("continue keyword used outside a loop"))
  | Try_catch(s1, _, s2) -> check_stmt fdecl context s1;
                            check_stmt fdecl context s2
  | Throw(e)             -> check_expr fdecl context e

(* check a func_decl and returns a func_decl_detail *)
let check_func fdecl =
  let context = { in_loop = false; variables = ref [] } in
  check_stmt fdecl context (Block(fdecl._body));
  { fname = fdecl._fname;
    formals = fdecl._formals;
    locals = diff_list !(context.variables) fdecl._formals;
    body = fdecl._body }

(* check a program and returns a program_detail *)
let check_program funcs =
  List.map check_func funcs