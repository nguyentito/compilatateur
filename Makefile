minic: src
	ocamlbuild -I src -use-menhir src/MiniC.native && cp MiniC.native minic

tests: minic
	runhaskell ./scripts/test.hs

.PHONY: clean

clean:
	ocamlbuild -clean; rm minic
