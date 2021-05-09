(*fichier lexeur.mll *)
{
	open Parseur
	exception Eof
	exception TokenInconu
}
rule token = parse
		([' ' '\t' '\n']|"//"_*|"/*"_*"*/") 			{ token lexbuf }
		| ';'                      			 { PT_VIRG }
		| ['0'-'9']+					{ NOMBRE }
		| (['0'-'9']+'.'['0'-'9']*|['0'-'9']*'.'['0'-'9']+)			{ NOMBRE }
		| "True" | "False"          { BOOL }
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
		| ['a'-'z'](['a'-'z'] | ['A'-'Z'] | '_' | ['0'-'9'])*   { VAR }
		| '='           { AFFECT }
		| "END"         { END }
		| eof 					{ raise Eof }
		| _ 						{ raise TokenInconu }
