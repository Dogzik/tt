open Grammar;;
open Buffer;;
open Printf;;

let (>>) x f = f x;;

let rec read_stdin_impl buff = 
  try
    begin
    let line = input_line stdin in
    add_string buff line;
    add_string buff "\n";
    read_stdin_impl buff;
    end
  with
    | End_of_file -> add_string buff ""
;;

let read_stdin () = begin
  let buff = create 100 in
  read_stdin_impl buff;
  contents buff
end;;

read_stdin () >> Lexing.from_string >> Parser.main Lexer.main >> string_of_expression >> fprintf stdout "%s\n";;

close_out stdout;;
close_in stdin;;
