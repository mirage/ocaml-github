.PHONY: all clean install build
PREFIX ?= /usr/local

all: build doc

setup.bin: setup.ml
	ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	rm -f setup.cmx setup.cmi setup.o setup.cmo

# do not overwrite an existing config.ml
lib_test/config.ml: lib_test/config.ml.in
	if [ ! -e lib_test/config.ml ]; then cp $< $@; else touch $@; fi

setup.data: setup.bin
	./setup.bin -configure --prefix $(PREFIX)

build: setup.data setup.bin lib_test/config.ml
	./setup.bin -build -j 4 -classic-display

doc: setup.data setup.bin
	./setup.bin -doc

install: setup.bin
	./setup.bin -install

test: setup.bin build
	./setup.bin -test

reinstall: setup.bin
	./setup.bin -reinstall

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log setup.bin
