OCAMLBUILD=ocamlbuild -use-ocamlfind 

all: 
	$(OCAMLBUILD) ohm.cma
	mv _build/ohm.cma _build/ohm.cmi ..

distclean: clean
	rm -rf _build || echo '' 

clean:
	$(OCAMLBUILD) -clean
	rm ../ohm.cma ../ohm.cmi || echo '' 
