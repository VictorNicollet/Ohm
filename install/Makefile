all: 
	ohm plugins.all # Run the DWIM mode of all plugin tools
	ohm assets # Build all the assets, place them in '/_build' 
	ohm build  # Build the ocaml code found in '/ocaml', generates '/ocaml/main.byte'

        # Copy the server binary over to the correct path
	ohm publish ocaml/main.byte /server.real 
	chmod a+x www/server.real

        # Publish all public elements
	ohm publish 

        # Perform the deployment
	www/server.real --put
	www/server.real --reset

clean: 
	ohm clean 
