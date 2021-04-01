MODULES=deck main author text command
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) 

play:
	clear && $(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip blackjack.zip *.ml* *.txt* *.json *.sh _tags .merlin .ocamlformat .ocamlinit LICENSE Makefile	
	
clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private blackjack.zip

