all:
	ocamllex lexeur.mll
	ocamlyacc parseur.mly
	ocamlc -c parseur.mli AST.ml lexeur.ml parseur.ml main.ml
	ocamlc -o main AST.cmo lexeur.cmo parseur.cmo main.cmo
