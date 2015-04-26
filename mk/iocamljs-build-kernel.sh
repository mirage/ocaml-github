#!/bin/sh -ex

# An example usage
#jsoo_mktop -dont-export-unit gc \
#   -top-syntax lwt.syntax \
#   -top-syntax js_of_ocaml.syntax \
#   -export-package lwt \
#   -export-package js_of_ocaml \
#   -export-package uuidm \
#   -export-package iocamljs-kernel \
#   -export-package dockerfile \
#   -jsopt +weak.js -jsopt +toplevel.js -o mycustom.byte

rm -rf _obj
mkdir _obj
cd _obj

NAME=dockerfile

jsoo_mktop -dont-export-unit gc \
  -export-package dockerfile \
  -export-package iocamljs-kernel \
  -jsopt +weak.js -jsopt +toplevel.js -o ${NAME}.byte

cat *.cmis.js \
  `opam config var lib`/iocamljs-kernel/kernel.js ${NAME}.js > \
  `opam config var share`/iocamljs-kernel/profile/static/services/kernels/js/kernel.${NAME}.js

cd ..
rm -rf _obj
