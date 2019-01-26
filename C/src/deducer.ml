open Solver;;
open Printf;;
open Grammar;;

module Ht = Hashtbl;;

let tab = "*   ";;

let get_ctx solution map_free = begin
  let pairs = Ht.fold (fun k v acc -> (k, v)::acc) map_free [] in
  let ind_to_type_s ind = term_to_string (apply_subst (Atom(ind)) solution) in
  let pair_to_elem (k, v) = String.concat " " [k; ":"; (ind_to_type_s v)] in 
  String.concat ", " (List.map pair_to_elem pairs)
end;;

let rec proof_inner expr solution map_free map_bond ctx cur_tab ind = match expr with
  | Var(Name(s)) -> begin
                      let id = match (Ht.find_opt map_bond s) with
                        | Some(x) -> x
                        | None -> Ht.find map_free s
                      in
                      let s_t = apply_subst (Atom(id)) solution in
                      let cur_proof = String.concat "" [cur_tab; ctx; " |- "; s; " : "; (term_to_string s_t); " [rule #1]"] in
                      (s, s_t, [cur_proof], (ind + 1))
                    end
  | Lambda(Name(s), p) -> begin
                            Ht.add map_bond s (ind + 1);
                            let s_t = apply_subst (Atom(ind + 1)) solution in
                            let s_t_s = term_to_string s_t in
                            let sep = if ((String.length ctx) = 0) then "" else ", " in
                            let tail_ctx = String.concat "" [ctx; sep; s; " : "; s_t_s] in
                            let (p_s, p_t, tail_proof, ind_1) = proof_inner p solution map_free map_bond tail_ctx (cur_tab ^ tab) (ind + 1) in
                            Ht.remove map_bond s;
                            let cur_s = String.concat "" ["(\\"; s; ". "; p_s; ")" ] in
                            let gap = if ((String.length ctx) = 0) then "" else " " in
                            let cur_step = String.concat "" [cur_tab; ctx; gap; "|- "; cur_s; " : ("; s_t_s; " -> "; (term_to_string p_t); ") [rule #3]"] in
                            let cur_proof = cur_step::tail_proof in
                            let cur_t = Impl(s_t, p_t) in
                            (cur_s, cur_t, cur_proof, ind_1)
                          end
  | Apl(p, q) ->  begin
                    let new_tab = cur_tab ^ tab in
                    let (p_s, p_t, p_proof, ind_1) = proof_inner p solution map_free map_bond ctx new_tab ind in
                    let (q_s, q_t, q_proof, ind_2) = proof_inner q solution map_free map_bond ctx new_tab ind_1 in
                    let tail_proof = List.rev_append (List.rev p_proof) q_proof in
                    let cur_s = String.concat "" ["("; p_s; " "; q_s; ")"] in
                    let cur_t = match p_t with
                      | Impl(a, b) -> b
                      | _ -> raise (Failure("KEK"));
                    in
                    let gap = if ((String.length ctx) = 0) then "" else " " in
                    let cur_step = String.concat "" [cur_tab; ctx; gap; "|- "; cur_s; " : "; (term_to_string cur_t); " [rule #2]"] in
                    let cur_proof = cur_step::tail_proof in
                    (cur_s, cur_t, cur_proof, ind_2 + 1)
                  end
;;


let get_proof expr solution map_free = begin
  let map_bond = (Ht.create 228 : (string, int) Ht.t) in
  let ctx = get_ctx solution map_free in
  let (cur_s, cur_t, cur_proof, ind) = proof_inner expr solution map_free map_bond ctx "" 0 in
  cur_proof
end;;





