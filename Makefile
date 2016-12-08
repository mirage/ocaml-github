JS=$(shell opam config var js_of_ocaml:installed)

.PHONY: build doc

all:
	ocaml pkg/pkg.ml build --with-unix true --with-js $(JS)

doc:
	topkg doc -r --build-flags ''
