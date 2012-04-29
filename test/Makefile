OCAMLBUILD=ocamlbuild -use-ocamlfind -lib ozone -cflags -I,../.. -lflags -I,../.. 

all: 
	make -C ../src all
	$(OCAMLBUILD) main.byte
	./main.byte

distclean: 
	$(OCAMLBUILD) -clean
	rm -rf _build || echo ''

clean:
	$(OCAMLBUILD) -clean

