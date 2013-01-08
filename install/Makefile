all: 
	ohm plugins.all # Run the DWIM mode of all plugin tools
	ohm assets # Build all the assets, place them in '/_build' 
	ohm build  # Build the ocaml code found in '/ocaml', generates '/ocaml/main.byte'

        # Copy the server binary over to the correct path
	cp ocaml/main.byte server
	chmod a+x server

        # Publish all public elements
	ohm publish 

        # Perform the deployment
	./server --put
	./server --reset

clean: 
	ohm clean 
