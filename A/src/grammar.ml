open Buffer;;


type variable = Name of string
type expression = Var of variable | Apl of expression * expression | Lambda of variable * expression

let string_of_variable_impl var buff = match var with
  | Name (s)  -> add_string buff s
;;

let rec string_of_expression_impl expr buff = match expr with
	| Var (var)       ->  string_of_variable_impl var buff
	| Apl (f, g)      ->  begin
                          add_string buff "(";
                          string_of_expression_impl f buff;
                          add_string buff " ";
                          string_of_expression_impl g buff;
                          add_string buff ")";
                        end
	| Lambda (var, f) ->  begin
                          add_string buff "(\\";
                          string_of_variable_impl var buff;   
                          add_string buff ".";
                          string_of_expression_impl f buff;
                          add_string buff ")";
                        end
;;

let string_of_expression expr = begin
  let buff = create 100 in
  string_of_expression_impl expr buff;
  contents buff
end;;
