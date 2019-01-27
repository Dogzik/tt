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

let print_debr expr = print_string ((de_bruijn_to_string expr) ^ "\n");;

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

let rec get_free_vars expr = match expr with
| BondVar(x) -> []
| FreeVar(s) -> [s]
| Lambda(p) -> get_free_vars p
| Apl(p, q) -> List.rev_append (get_free_vars p) (get_free_vars q)

let get_vacant_name expr = begin
  let free_vars = get_free_vars expr in
  let biggest = List.fold_left (fun a b -> if ((String.compare a b) < 0) then b else a) "" free_vars in
  (String.map (fun c -> 't') biggest) ^ "t"
end;;

let rec de_bruijn_to_output_inner expr arg buff ind = match expr with
| FreeVar(s) -> Buffer.add_string buff s;
| BondVar(x) -> begin
                  Buffer.add_string buff arg;
                  Buffer.add_string buff (string_of_int (ind - x - 1));
                end;
| Apl(p, q) ->  begin
                  Buffer.add_string buff "(";
                  de_bruijn_to_output_inner p arg buff ind;
                  Buffer.add_string buff " ";
                  de_bruijn_to_output_inner q arg buff ind;
                  Buffer.add_string buff ")";
                end;
| Lambda(p) ->  begin
                  Buffer.add_string buff "(\\";
                  Buffer.add_string buff arg;
                  Buffer.add_string buff (string_of_int ind);
                  Buffer.add_string buff ". ";
                  de_bruijn_to_output_inner p arg buff (ind + 1);
                  Buffer.add_string buff ")";
                end
;;

let de_bruijn_to_output expr arg = begin
  let buff = Buffer.create 1337 in
  de_bruijn_to_output_inner expr arg buff 0;
  Buffer.contents buff
end;;
