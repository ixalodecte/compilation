all:
	ocamllex lexeur.mll
	ocamlyacc parseur.mly
	ocamlc -c AST.ml
	ocamlc -o AST.cmo
	ocamlc -c parseur.mli AST.ml lexeur.ml parseur.ml main.ml
	ocamlc -o main AST.cmo lexeur.cmo parseur.cmo main.cmo

clean:
	rm parseur.mli lexeur.ml parseur.ml main AST.cmo lexeur.cmo parseur.cmo main.cmo parseur.cmi main.cmi lexeur.cmi
