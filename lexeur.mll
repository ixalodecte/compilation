(*fichier lexeur.mll *)
{
	open Parseur
	exception Eof
	exception TokenInconu
}
rule token = parse
<<<<<<< HEAD
		[' ' '\t' '\n'] 			{ token lexbuf }
=======
		([' ' '\t' '\n']) 			{ token lexbuf }
>>>>>>> parser
		| "//"(_#'\n')*   { token lexbuf }
		| "/*"((_#'*')|('*'+(_#['/' '*'])))*'*'+'/'   { token lexbuf }
		| ';'                      			 { PT_VIRG }
		| ['0'-'9']+	as lexem				{ NOMBRE(float_of_string lexem) }
		| (['0'-'9']+'.'['0'-'9']*|['0'-'9']*'.'['0'-'9']+)	as lexem		{ NOMBRE(float_of_string lexem) }
		| "True" | "False" as lexem         { BOOL(bool_of_string (String.lowercase_ascii lexem)) }
		| "=="     { EGAL }
		| ">="      { SUP_EGAL }
		| '!'       { NON }
		| '>'       { SUP }
		| '+' 			 			{ PLUS }
		| '-' 						{ MOINS }
		| '*' 						{ FOIS }
		| '/'							{ DIV }
		| '(' 						{ GPAREN }
		| ')'						{ DPAREN }
		| ['a'-'z'](['a'-'z'] | ['A'-'Z'] | '_' | ['0'-'9'])* as lexem  { VAR(lexem) }
		| '='           { AFFECT }
		| "END"         { END }
		| eof 					{ raise Eof }
		| _ 						{ raise TokenInconu }
