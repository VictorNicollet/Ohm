OCAMLBUILD=ocamlbuild -use-ocamlfind 

all: 
	$(OCAMLBUILD) tool.byte

distclean: 
	$(OCAMLBUILD) -clean
	rm -rf _build || echo ''

clean:
	$(OCAMLBUILD) -clean

