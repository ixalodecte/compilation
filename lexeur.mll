(*fichier lexeur.mll *)
{
	open Parseur
	exception Eof
	exception TokenInconu
}
rule token = parse
		[' ' '\t' '\n'] 				{ token lexbuf }
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
		| eof 					{ raise Eof }
		| _ 						{ raise TokenInconu }
