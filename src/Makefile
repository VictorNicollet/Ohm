OCAMLBUILD=ocamlbuild -X p4 -use-ocamlfind

all: 
	make -C ../p4 ../pp.cmo
	$(OCAMLBUILD) ohm.cma
	mv _build/ohm.cma _build/ohm.cmi ..

distclean: clean
	rm -rf _build || echo '' 

clean:
	$(OCAMLBUILD) -clean
	rm ../ohm.cma ../ohm.cmi || echo '' 
