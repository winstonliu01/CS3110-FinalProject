MODULES= deck main author text command player round 
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

docs: docs-public docs-private

docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d _doc.public $(MLIS)  

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)
clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private blackjack.zip

