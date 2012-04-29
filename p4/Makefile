all: ../pp.cmo

clean:
	rm -f *.cmo *.cmi ../pp.cmo ../pp.cmi

test: test.ml ../pp.cmo
	ocamlfind ocamlc -syntax camlp4o -package camlp4 -ppopt "../pp.cmo" test.ml

distclean: 
	rm -f *.cmo *.cmi
	rm -f *~

../pp.cmo: pp.ml
	ocamlc -pp 'camlp4o q_MLast.cmo pa_extend.cmo' -I +camlp4 -c pp.ml
	mv pp.cmo pp.cmi ..