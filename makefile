all: run

run: run.ml
	ocamlbuild -use-ocamlfind run.byte