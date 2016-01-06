#! /bin/sh

oasis setup
ocaml setup.ml -configure --enable-tests
ocaml setup.ml -all
case "$1" in
    "-install") ocaml setup.ml -reinstall;;
esac
