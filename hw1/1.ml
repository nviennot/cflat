(*
let rec rmdup list
  [] -> []
| head::tail -> if head = (List.hd tail) then rmdup (List.tl list)
                                  else head::(rmdup (List.tl list))
;;
*)

let rmdup2 list =
  (* Build done backwards *)
  let rec helper don = function
    []   -> don
  | h::t -> if (List.hd don) = h then (helper don t) else (helper h::don t)
  in
  List.rev (helper [] list)
;;

let rec rmdup list =
  if list = [] then
    []
  else
    let head = List.hd list in
    let d = rmdup (List.tl list) in
    if d = [] then
      d
    else
      if head = (List.hd d) then
        d
      else
        head::d
;;


(*
let litos l = List.Map string_of_int l;;
let liprint l = List.Iter print_endline (litos l);;
*)

let f i = print_int i; print_string ", ";;
let p l = List.iter f l; print_newline ();;

let l = [1; 2; 2; 2; 3; 3];;
p l;;

rmdup2 l;;

p l;;
