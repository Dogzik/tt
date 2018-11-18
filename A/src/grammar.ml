open Buffer;;


type variable = Name of string
type expression = Var of variable | Apl of expression * expression | Lambda of variable * expression

let string_of_variable var buff = match var with
  | Name (s)  -> add_string buff s

let rec string_of_expression expr buff = match expr with
	| Var (var)       ->  string_of_variable var buff
	| Apl (f, g)      ->  begin
                          add_string buff "(";
                          string_of_expression f buff;
                          add_string buff " ";
                          string_of_expression g buff;
                          add_string buff ")";
                        end
	| Lambda (var, f) ->  begin
                          add_string buff "(\\";
                          string_of_variable var buff;   
                          add_string buff ".";
                          string_of_expression f buff;
                          add_string buff ")";
                        end 
