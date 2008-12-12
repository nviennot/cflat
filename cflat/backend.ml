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
  |  Id(s) -> sprintf "mov eax, [ebp+%d]\n" (id_to_offset fdecl s)
  |  Binop(e1, o, e2) ->
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
		  Add -> 	"add eax, ecx\n"
		| Sub -> 	"sub eax, ecx\n"
		| Mult ->	"imul eax, ecx\n"
		| Div -> 	"cdq\n" ^
				"idiv ecx\n"

		| Equal ->	"sete  al\n"
		| Neq	->	"setne al\n"
		| Less	->	"setl  al\n"
		| Leq	-> 	"setle al\n"
		| Greater ->	"setg  al\n"
		| Geq	->	"setge al\n")

  |  Assign(v, e) ->
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
      let l1 = get_new_label context
      and l2 = get_new_label context in
      String.concat "\n" [
      eval_expr_to_eax fdecl e;
      "test eax, eax";
      "jz " ^ l1;
      (string_of_stmt context fdecl s1);
      "jmp " ^ l2;
      l1 ^ ":";
      (string_of_stmt context fdecl s2);
      l2 ^ ":"] ^ "\n"

  | For(e1, e2, e3, s) ->
	let l1 = get_new_label context
	and l2 = get_new_label context
	and l3 = get_new_label context in
	let context' = { context with continue_label = Some l2;
			 break_label = Some l3 } in
	eval_expr_to_eax fdecl e1 ^
	"jmp " ^ l1 ^ "\n" ^
	l2 ^ ":\n" ^
	eval_expr_to_eax fdecl e3 ^
	l1 ^ ":\n" ^
	eval_expr_to_eax fdecl e2 ^
	"test eax, eax\n" ^
	"jz " ^ l3 ^ "\n" ^
	string_of_stmt context' fdecl s ^
	"jmp " ^ l2 ^ "\n" ^
	l3 ^ ":\n"

  | While(e, s) -> string_of_stmt context fdecl (For(Noexpr, e, Noexpr, s))
  | Break -> "jmp " ^ get context.break_label ^ "\n"
  | Continue -> "jmp " ^ get context.continue_label ^ "\n"

let string_of_vdecl id = "int " ^ id ^ ";\n"

let string_of_fdecl context fdecl =
let context' = { context with return_label = Some (get_new_label context) } in
  ".globl " ^ fdecl.fname ^ "\n" ^
  "        .type   " ^ fdecl.fname ^ ", @function\n" ^
  fdecl.fname ^ ":\n" ^
  "        push    ebp\n" ^
  "        mov     ebp, esp\n" ^
  "        sub     esp, " ^ (string_of_int (4 * (List.length fdecl.locals))) ^ "\n" ^
  "        push    ecx\n" ^
  "        push    edx\n" ^
  String.concat "" (List.map (string_of_stmt context' fdecl) fdecl.body) ^
  get context'.return_label ^ ":\n" ^
  "        pop     edx\n" ^
  "        pop     ecx\n" ^
  "        mov     esp, ebp\n" ^
  "        pop     ebp\n" ^
  "        ret\n"

let generate_asm (vars, funcs) =
  let context = { label_count = ref 0;
                  continue_label = None;
                  break_label = None;
                  return_label = None} in
  "        .intel_syntax\n        .text\n        .intel_syntax noprefix\n" ^
  String.concat "\n" (List.map (string_of_fdecl context) funcs) ^
  "        .ident  \"C Flat compiler 0.1\"\n        .section        .note.GNU-stack,\"\",@progbits\n"
