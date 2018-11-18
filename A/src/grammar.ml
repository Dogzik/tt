type variable = Name of string
type expression = Var of variable | Apl of expression * expression | Lambda of variable * expression

let string_of_variable = function
  | Name (s)  -> s

let rec string_of_expression = function
	| Var (var)           -> string_of_variable var
	| Apl (f, g)          -> "(" ^ (string_of_expression f) ^ " " ^ (string_of_expression g) ^ ")"
	| Lambda (var, f) -> "(\\" ^ (string_of_variable var) ^ "." ^ (string_of_expression f) ^ ")" 
