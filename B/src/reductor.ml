open De_bruijn;;

let rec subst_inner expr arg depth = match expr with
| FreeVar(s) -> expr
| BondVar(x) -> (match (compare x depth) with
                 | 0 -> arg
                 | a when (a > 0) -> BondVar(x - 1)
                 | b when (b < 0) -> expr
                )
| Lambda(p) ->  Lambda(subst_inner p arg (depth + 1))
| Apl(p, q) -> Apl((subst_inner p arg depth), (subst_inner q arg depth))

let subst expr arg = match expr with
| Lambda(p) -> subst_inner p arg 0
| _ -> expr
;;

let rec reduct expr = match expr with
| Apl(a, b) -> (match a with
                | Lambda(p) -> reduct (subst a b)
                | _ ->  begin
                          let a_red = reduct a in
                          match a_red with
                          | Lambda(q) -> reduct (subst a_red b)
                          | _ -> Apl(a_red, reduct(b))
                        end
                )
| Lambda(p) -> Lambda(reduct p)
| FreeVar(x) -> expr
| BondVar(s) -> expr
