open Grammar;;
open Buffer;;
open Printf;;
open Solver;;
open Deducer;;

module Ht = Hashtbl;;

let (>>) x f = f x;;

let rec read_stdin_impl buff = 
  try
    begin
    let line = input_line stdin in
    add_string buff line;
    add_string buff "\n";
    read_stdin_impl buff
    end
  with
    | End_of_file -> buff
;;

let read_stdin () = begin
  let buff1 = create 1337 in
  let buff2 = read_stdin_impl buff1 in
  contents buff2
end;;

let expr = read_stdin () >> Lexing.from_string >> Parser.main Lexer.main;;

let map_free = (Ht.create 228 : (string, int) Ht.t);;
let map_bond = (Ht.create 228 : (string, int) Ht.t);;
let (e, t, ind) = get_system expr 0 map_free map_bond;;

let substed = (Ht.create 228 : (alg_term, bool) Ht.t);;
match (solve_system e substed) with
  | None -> fprintf stdout "Expression has no type\n";
  | Some(solution) -> begin
                        let proof = get_proof expr solution map_free in
                        List.iter (fun line -> fprintf stdout "%s\n" line) proof;
                      end;
close_out stdout;;
close_in stdin;;

