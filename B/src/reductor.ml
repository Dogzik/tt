open De_bruijn;;

let rec arg_fix arg ind fixer = match arg with
| FreeVar(s) -> arg
| BondVar(x) -> if (x >= ind) then BondVar(x + fixer) else arg
| Apl(p, q) -> Apl((arg_fix p ind fixer), (arg_fix q ind fixer))
| Lambda(p) -> Lambda(arg_fix p (ind + 1) fixer)
;;

let rec subst_inner expr arg depth = match expr with
| FreeVar(s) -> expr
| BondVar(x) -> (match (compare x depth) with
                 | 0 -> arg_fix arg 0 depth
                 | a when (a > 0) -> BondVar(x - 1)
                 | b when (b < 0) -> expr
                )
| Lambda(p) ->  Lambda(subst_inner p arg (depth + 1))
| Apl(p, q) -> Apl((subst_inner p arg depth), (subst_inner q arg depth))

let subst expr arg = match expr with
| Lambda(p) -> subst_inner p arg 0
| _ -> expr
;;

(*
let rec reduct expr = begin
  (*print_debr expr;*)
  match expr with
  | Apl(a, b) -> (match a with
                  | Lambda(p) -> reduct (subst a b)
                  | _ ->  begin
                            let a_red = reduct a in
                            match a_red with
                            | Lambda(q) -> reduct (subst a_red b)
                            | _ ->  Apl(a_red, (reduct b))
                          end
                  )
  | Lambda(p) -> Lambda(reduct p)
  | FreeVar(x) -> expr
  | BondVar(s) -> expr
end;;
*)

let rec reduct expr = begin
  let rec reduct_inner expr = match expr with
    | Apl(a, b) -> (match a with
                    | Lambda(p) -> ((subst a b), true)
                    | _ ->  begin
                              let (a_red, change_a) = reduct_inner a in
                              if (change_a) then (Apl(a_red, b), true) else (Apl(a, reduct b), false)
                            end
                    )
    | Lambda(p) ->  begin
                    let (p_red, changed) = reduct_inner p in
                    (Lambda(p_red), changed)
                    end
    | FreeVar(x) -> (expr, false)
    | BondVar(s) -> (expr, false)
  in
  let (x, t) = reduct_inner expr in
  let e = ref x in
  let f = ref t in
  while (!f) do
    let (xx, tt) = reduct_inner !e in
    e := xx;
    f := tt;
  done;
  !e
end;;
