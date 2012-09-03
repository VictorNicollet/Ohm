OHM=ohm # The name of the 'ohm' tool to be invoked

all: 
	$(OHM) plugins.all # Run the DWIM mode of all installed plugin tools
	$(OHM) assets # Build all the assets, place them in '/_build' 
	$(OHM) build  # Build the ocaml code found in '/ocaml', generates '/ocaml/main.byte'

        #Copy the server binary over to the correct path
	cp ocaml/main.byte www/server 
	mv www/server www/server.real 

        # Perform the deployment
	www/server.real --put
	www/server.real --reset

clean: 
	$(OHM) clean 