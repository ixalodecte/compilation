all:
	ocamllex lexeur.mll
	ocamlyacc parseur.mly
	ocamlc -c parseur.mli lexeur.ml parseur.ml main.ml
	ocamlc -o main lexeur.cmo parseur.cmo main.cmo

clean:
	rm parseur.mli lexeur.ml parseur.ml main lexeur.cmo parseur.cmo main.cmo parseur.cmi main.cmi lexeur.cmi
