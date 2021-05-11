(*fichier lexeur.mll *)
{
	open Parseur
	exception Eof
	exception TokenInconu
}
rule token = parse
		([' ' '\t' '\n']) 			{ token lexbuf }
		| "//"(_#'\n')*   { token lexbuf }
		| "/*"((_#'*')|('*'+(_#['/' '*'])))*'*'+'/'   { token lexbuf }
		| ';'                      			 { PT_VIRG }
		| ['0'-'9']+('e''-'?['0'-'9']+)?					{ NOMBRE }
		| (['0'-'9']+'.'['0'-'9']*|['0'-'9']*'.'['0'-'9']+)('e''-'?['0'-'9']+)?			{ NOMBRE }
		| "True" | "False"          { BOOL }
		| "=="     { EGAL }
		| "++"     { INCREM }
		| ">="      { SUP_EGAL }
		| '!'       { NON }
		| '>'       { SUP }
		| "&&"      { AND }
		| "||"      { OR }
		| '+' 			 			{ PLUS }
		| '-' 						{ MOINS }
		| '*' 						{ FOIS }
		| '/'							{ DIV }
		| '(' 						{ GPAREN }
		| ')'						{ DPAREN }
		| ['a'-'z'](['a'-'z'] | ['A'-'Z'] | '_' | ['0'-'9'])*   { VAR }
		| '='           { AFFECT }
		| "END"         { END }
		| "If"          { IF }
		| '{'           { GACC }
		| '}'           { DACC }
		| "Else"        { ELSE}
		| "Do"          { DO }
		| "While"       { WHILE }
		| "For"         { FOR }
		| eof 					{ raise Eof }
		| _ 						{ raise TokenInconu }
