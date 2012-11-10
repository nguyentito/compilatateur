Projet de compilateur
=====================

Instructions pour builder
-------------------------

Un jour, il y aura un Makefile, et on pourra faire "make" pour compiler
et "make clean" pour nettoyer le répertoire. En attendant, se débrouiller
avec ocamlbuild...

Suite de tests
--------------

Pour lancer les tests, compiler test.ml avec la commande

    ocamlbuild -use-menhir -libs str test.native

puis taper la commande ./test.native.
