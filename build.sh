#! /bin/sh

if [ ! -f setup.ml ]; then
    oasis setup
    ocaml setup.ml -configure --enable-tests
fi
ocaml setup.ml -all
case "$1" in
    "-install") ocaml setup.ml -reinstall;;
esac
