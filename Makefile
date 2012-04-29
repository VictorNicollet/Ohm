all:
	make -C p4
	make -C src
	make -C tool

clean:
	make -C p4 clean
	make -C src clean
	make -C tool clean