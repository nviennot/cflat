open Ast
open Printf

type context = {
  in_loop  : bool;
}

let rec merge_unique l1 = function
    [] -> l1
  | hd2 :: tl2 ->
      let rec merge_unique_hd2 = function
          [] -> [hd2]
        | hd1 :: tl1 ->
            if hd1 = hd2 then hd1 :: tl1
            else hd1 :: merge_unique_hd2 tl1 in
  merge_unique (merge_unique_hd2 l1) tl2

let rec diff_list l1 = function
    [] -> l1
  | hd2 :: tl2 ->
    let rec diff_hd2 = function
        [] -> []
      | hd1 :: tl1 ->
          if hd1 = hd2 then diff_hd2 tl1
          else hd1 :: diff_hd2 tl1 in
    diff_list (diff_hd2 l1) tl2

(* check_expr/check_stmt returns a list of used variables *)
let rec check_expr fdecl = function
    Literal(_) -> []
  | Id(v) -> [v]
  | Unop(_, e) -> check_expr fdecl e
  | Incop(_, v) -> [v]
  | Binop(e1, _, e2) -> merge_unique (check_expr fdecl e1) (check_expr fdecl e2)
  | Assignop(v, _, e) -> merge_unique [v] (check_expr fdecl e)
  | Assign (v, e) -> merge_unique [v] (check_expr fdecl e)
  | Call(_, el) ->
      List.fold_left merge_unique [] (List.map (check_expr fdecl) el)
  | Noexpr -> []

let rec check_stmt fdecl context = function
    Block(sl) ->
      List.fold_left merge_unique [] (List.map (check_stmt fdecl context) sl)
  | Expr(e) -> check_expr fdecl e
  | Return(e) -> check_expr fdecl e
  | If(e, s1, s2) ->
      merge_unique
        (merge_unique (check_expr fdecl e) (check_stmt fdecl context s1))
        (check_stmt fdecl context s2)
  | For(e1, e2, e3, s) ->
      let context' = { in_loop = true } in
      List.fold_left merge_unique []
        [check_expr fdecl e1; check_expr fdecl e2; check_expr fdecl e3;
        check_stmt fdecl context' s]
  | While(e, s) ->  check_stmt fdecl context (For(Noexpr, e, Noexpr, s))
  | Break ->
      if not context.in_loop then
        raise (Failure ("break keyword used outside a loop"))
      else []
  | Continue ->
      if not context.in_loop then
        raise (Failure ("continue keyword used outside a loop"))
        else []
  | Try_catch(s1, _, s2) ->
     merge_unique (check_stmt fdecl context s1) (check_stmt fdecl context s2)
  | Throw(e) -> check_expr fdecl e

(* check a func_decl and returns a func_decl_detail *)
let check_func fdecl =
  let context = { in_loop = false } in
  let variables = check_stmt fdecl context (Block(fdecl._body)) in
  { fname = fdecl._fname; formals = fdecl._formals;
    locals = diff_list variables fdecl._formals;
    body = fdecl._body}

(* check a program and returns a program_detail *)
let check_program funcs =
  List.map check_func funcs