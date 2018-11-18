open Grammar;;
open Buffer;;
open Printf;;

let (>>) x f = f x;;

let rec read_stdin () = 
  try
    begin
    let line = input_line stdin in
    line ^ "\n" ^ read_stdin ()
    end
  with
    |End_of_file -> "";;


let input = read_stdin ();;
let lexed = Lexing.from_string input;;
let parsed = Parser.main Lexer.main lexed;;
let res = string_of_expression parsed;;
fprintf stdout "%s\n" res;;
(*
read_stdin () >> Lexing.from_string >> Parser.main Lexer.main >> string_of_expression >> fprintf stdout "%s\n";;
*)

close_out stdout;;
close_in stdin;;
