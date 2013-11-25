all: run

run: util_bmp.ml open_bmp.ml fonte_bmp.ml run.ml
	ocamlopt -thread unix.cmxa threads.cmxa graphics.cmxa $^ -o run

clean:
	rm -f *.cm[ioxa] run *.o

