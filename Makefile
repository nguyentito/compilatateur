minic: lexer.mll parser.mly ast.mli typing.ml main.ml
	ocamlbuild -use-menhir main.native && cp main.native minic

tests: minic
	ocamlbuild -libs str,unix test.native && ./test.native

.PHONY: clean

clean:
	ocamlbuild -clean; rm minic
