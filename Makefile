minic: lexer.mll parser.mly ast.mli typing.ml main.ml
	ocamlbuild -use-menhir main.native && cp main.native minic

tests: lexer.mll parser.mly ast.mli typing.ml test.ml
	ocamlbuild -use-menhir -libs str test.native && ./test.native

.PHONY: clean

clean:
	ocamlbuild -clean; rm minic

