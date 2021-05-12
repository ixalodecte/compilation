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
		| "true" | "false"          { BOOL }
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
		| "END"         { END }
		| "if"          { IF }
		| "else"        { ELSE}
		| "do"          { DO }
		| "while"       { WHILE }
		| "for"         { FOR }
		| "undefined"   { UNDEF }
		| "NaN"         { NAN }
		| "function"    { FUNC }
		| "return"      { RETURN }
		| ','           { VIRG }
		| (['a'-'z']| ['A'-'Z'])(['a'-'z'] | ['A'-'Z'] | '_' | ['0'-'9'])*  { VAR }
		| '='           { AFFECT }
		| '{'           { GACC }
		| '}'           { DACC }
		| eof 					{ raise Eof }
		| _ 						{ raise TokenInconu }
