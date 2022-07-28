#!/usr/bin/env bash

help() {
  echo "$0 [OPTIONS]"
  echo -e "-h | --help\t\tHelp information"
  echo -e "-r | --run\t\tRun the game"
  echo -e "-c | --compile\t\tCompile the game"
}

case $1 in
  "-r"|"--run")
    echo "Running..."
    sbcl --noinform --load mun.asd --eval "(require :mun)" --eval "(mun:run)" --eval "(exit)"
    ;;
  "-c"|"--compile")
    sbcl --noinform --load mun.asd --eval "(require :mun)" --eval "(sb-ext:save-lisp-and-die \"mun\" :toplevel #'mun:run :executable t)" --eval "(exit)"
    echo "Compiled!"
    ;;
  "-h"|"--help")
    help
    ;;
  *)
    help ;;
esac

