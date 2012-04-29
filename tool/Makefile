OCAMLBUILD=ocamlbuild -use-ocamlfind -lib ohm -cflags -I,../.. -lflags -I,../.. 

all: 
	make -C ../src all
	$(OCAMLBUILD) tool.byte

distclean: 
	$(OCAMLBUILD) -clean
	rm -rf _build || echo ''

clean:
	$(OCAMLBUILD) -clean

