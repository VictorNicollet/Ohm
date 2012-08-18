Ohm
===

Ohm is a web framework for the OCaml language, open sourced under the MIT license. It is 
currently in alpha, with the public beta release scheduled for September 2012.

Installation
------------

To use the framework, you must install the `ohm` command-line tool.

Place files [`install.sh`](https://github.com/VictorNicollet/Ohm/blob/master/install.sh) 
and [`run.ml`](https://github.com/VictorNicollet/Ohm/blob/master/run.ml) in a directory,
then run `./install.sh` with administrator privileges :

    wget https://raw.github.com/VictorNicollet/Ohm/master/install.sh
    wget https://raw.github.com/VictorNicollet/Ohm/master/run.ml 
    sudo ./install.sh

Once this is done, you can create a brand new project by using the `ohm init` command : 

    ohm init my-project

This creates directory `my-project`, downloads a fresh and up-to-date copy of the 
Ohm framework, sets up the files required for a minimalistic project, and compiles it. 

Requirements
------------

Ohm requires OCaml 3.12.0 or later, as well as `ocamlfind` and `ocamlbuild`. It also 
relies on the following OCaml libraries : 

  - [OCurl](http://sourceforge.net/projects/ocurl/) 
  - [Batteries](http://forge.ocamlcore.org/projects/batteries) 
  - [OCamlNet 3](http://projects.camlcity.org/projects/ocamlnet.html/)
  - [OCaml-SHA](https://github.com/vincenthz/ocaml-sha) 
  - [Xmlm](http://caml.inria.fr/cgi-bin/hump.en.cgi?contrib=563) 

There may be attempts to eliminate somme of these requirements later on.

Ohm is designed to sit behind any FastCGI-enabled server, but has only been tested
with Apache so far.

Ohm uses [CouchDB](http://couchdb.apache.org/) as its primary database (there is work 
underway to allow the use of [PostgreSQL](http://www.postgresql.org/) as well). 

The asset pipeline uses LESS CSS and Coffeescript. 

On a debian system, use the following command-line to grab all the packages required to
run Ohm : 

    apt-get install ocaml-nox ocaml-findlib ocamldsort ocaml-native-compilers \
        libsha-ocaml-dev libbatteries-ocaml-dev libocamlnet-ocaml-dev \
        libcurl-ocaml-dev libxmlm-ocaml-dev libapache2-mod-fastcgi make \
        couchdb coffeescript node-less
