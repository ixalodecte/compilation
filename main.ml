let _ = 									(*main en OCaml*)
	try
		let lexbuf = Lexing.from_channel stdin in 	(*lexeur lancé sur stdin*)
		while true do 							(*one ne s'arrête pas*)
			Parseur.main Lexeur.token lexbuf 		(*parseur une ligne*)
		done
	with
	  | Lexeur.Eof -> exit 0						 (*impossible*)
      	  | Lexeur.TokenInconu 						(*erreur de lexing*)
	  | Parsing.Parse_error ->						 (*erreur de parsing*)
		Printf.printf ("Ceci n'est pas une expression arithmetique\n")
