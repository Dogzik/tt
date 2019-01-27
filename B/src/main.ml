open Grammar;;
open Buffer;;
open Printf;;
open De_bruijn;;
open Reductor;;

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

let d_expr = to_de_bruijn expr;;
let norm = reduct d_expr;;
let bond_name = get_vacant_name norm;;
(*print_string ((de_bruijn_to_string norm) ^ "\n");;*)
print_string ((de_bruijn_to_output norm bond_name) ^ "\n");;
close_out stdout;;
close_in stdin;;
