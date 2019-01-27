open Grammar;;

type de_bruijn = FreeVar of string | BondVar of int | Apl of de_bruijn * de_bruijn | Lambda of de_bruijn;;

let rec de_bruijn_to_string_inner expr buff = match expr with
| BondVar(x) -> Buffer.add_string buff (string_of_int x);
| FreeVar(s) ->  Buffer.add_string buff s;
| Apl(p, q) ->  begin
                  Buffer.add_string buff "(";
                  de_bruijn_to_string_inner p buff;
                  Buffer.add_string buff " ";
                  de_bruijn_to_string_inner q buff;
                  Buffer.add_string buff ")";
                end;
| Lambda(p) ->  begin
                  Buffer.add_string buff "(λ. ";
                  de_bruijn_to_string_inner p buff;
                  Buffer.add_string buff ")";
                end
;;

let de_bruijn_to_string expr = begin
  let buff = Buffer.create 228 in
  de_bruijn_to_string_inner expr buff;
  Buffer.contents buff
end;;

let rec find_bond_inner x bonds ind = match bonds with
| head::tail -> if (head = x) then Some(ind) else find_bond_inner x tail (ind + 1)
| [] -> None
;;

let find_bond x bonds = find_bond_inner x bonds 0;;

let rec to_de_bruijn_inner expr bonds = match expr with
| Var(Name(s)) -> (match find_bond s bonds with
                  | Some(x) -> BondVar(x)
                  | None -> FreeVar(s)
                  )
| Apl(p, q) -> Apl((to_de_bruijn_inner p bonds), (to_de_bruijn_inner q bonds))
| Lambda(Name(s), p) -> Lambda(to_de_bruijn_inner p (s::bonds))
;;

let to_de_bruijn expr = to_de_bruijn_inner expr [];;
