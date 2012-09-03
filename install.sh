#!/bin/sh
ocamlc run.ml -o /usr/local/bin/ohm && chmod a+x /usr/local/bin/ohm && echo "Ohm installed !" && echo "Run 'ohm init <directory>' to create a new project" && rm run.cmo run.cmi 