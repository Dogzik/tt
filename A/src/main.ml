open Grammar;;
open Buffer;;
open Printf;;

let (>>) x f = f x;;

let rec read_stdin buff = 
  try
    begin
    let line = input_line stdin in
    add_string buff line;
    add_string buff "\n";
    read_stdin buff;
    end
  with
    |End_of_file -> add_string buff ""
;;


let in_buff = create 100;;
let out_buff = create 100;;
read_stdin in_buff;;
let lexed = Lexing.from_string (contents in_buff);;
let parsed = Parser.main Lexer.main lexed;;
string_of_expression parsed out_buff;;
let res = contents out_buff;;
fprintf stdout "%s\n" res;;
(*
read_stdin () >> Lexing.from_string >> Parser.main Lexer.main >> string_of_expression >> fprintf stdout "%s\n";;
*)

close_out stdout;;
close_in stdin;;
