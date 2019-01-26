open Solver;;
open Grammar;;

module Ht = Hashtbl;;

let tab = "*   ";;
let rec print_tabs cnt = match cnt with
  | 0 ->  ();
  | _ ->  begin
            print_string tab;
            print_tabs (cnt - 1);
          end
;;


let rec get_free_vars_inner expr = match expr with
  | Var(Name(s)) -> [s]
  | Apl(p, q) -> List.rev_append (get_free_vars_inner p) (get_free_vars_inner q)
  | Lambda(Name(s), p) -> List.filter (fun x -> not(x = s)) (get_free_vars_inner p)
;;

let get_ctx expr solution map = begin
  let vars = get_free_vars_inner expr in
  let uniq_vars = List.sort (String.compare) vars in
  let var_to_type var = term_to_string (apply_subst (Atom(Ht.find map var)) solution) in
  String.concat ", " (List.map (fun var -> var ^ " : " ^ (var_to_type var)) uniq_vars)
end;;

let rec proof_inner expr solution map ctx cur_tab = match expr with
  | Var(Name(s)) -> begin
                      let s_t = apply_subst (Atom(Ht.find map s)) solution in
                      (s, s_t, [String.concat "" [cur_tab; ctx; " |- "; s; " : "; (term_to_string s_t); " [rule #1]"]])
                    end
  | Lambda(Name(s), p) -> begin
                            let s_t = apply_subst (Atom(Ht.find map s)) solution in
                            let s_t_s = term_to_string s_t in
                            let sep = if ((String.length ctx) = 0) then "" else ", " in
                            let tail_ctx = String.concat "" [ctx; sep; s; " : "; s_t_s] in
                            let (p_s, p_t, tail_proof) = proof_inner p solution map tail_ctx (cur_tab ^ tab) in
                            let cur_s = String.concat "" ["(\\"; s; ". "; p_s; ")" ] in
                            let gap = if ((String.length ctx) = 0) then "" else " " in
                            let cur_step = String.concat "" [cur_tab; ctx; gap; "|- "; cur_s; " : ("; s_t_s; " -> "; (term_to_string p_t); ") [rule #3]"] in
                            let cur_proof = cur_step::tail_proof in
                            let cur_t = Impl(s_t, p_t) in
                            (cur_s, cur_t, cur_proof)
                          end
  | Apl(p, q) ->  begin
                    let new_tab = cur_tab ^ tab in
                    let (p_s, p_t, p_proof) = proof_inner p solution map ctx new_tab in
                    let (q_s, q_t, q_proof) = proof_inner q solution map ctx new_tab in
                    let tail_proof = List.rev_append (List.rev p_proof) q_proof in
                    let cur_s = String.concat "" ["("; p_s; " "; q_s; ")"] in
                    let cur_t = match p_t with
                      | Impl(a, b) -> b
                      | _ -> raise Not_found;
                    in
                    let gap = if ((String.length ctx) = 0) then "" else " " in
                    let cur_step = String.concat "" [cur_tab; ctx; gap; "|- "; cur_s; " : "; (term_to_string cur_t); " [rule #2]"] in
                    let cur_proof = cur_step::tail_proof in
                    (cur_s, cur_t, cur_proof)
                  end
;;


let get_proof expr solution map = begin
  let ctx = get_ctx expr solution map in
  let (cur_s, cur_t, cur_proof) = proof_inner expr solution map ctx "" in
  cur_proof
end;;





