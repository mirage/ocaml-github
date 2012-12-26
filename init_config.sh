#!/bin/sh
if [ ! -e ./lib_test/config.ml ]; then
  cp ./lib_test/config.ml.in ./lib_test/config.ml
fi
