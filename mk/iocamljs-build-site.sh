#!/bin/sh -ex

NAME=dockerfile
BASEURL=/ocaml-dockerfile

iocaml -js ${NAME} notebooks/ \
    -create-static-site html \
    -static-site-base-path "${BASEURL}"
