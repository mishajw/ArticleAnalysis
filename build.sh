#!/bin/bash

action=$1

if [ -z $1 ]; then
  action="all"
fi

do_build() {
  stack build --ghc-options="-W"
}

do_test() {
  stack test
}

do_run() {
  stack exec ArticleAnalysis-exe res/txts/ 3
}

case $action in
  "build")
    do_build ;;
  "run")
    do_build && do_run ;;
  "test")
    do_test ;;
  "all")
    if do_build; then
      do_run
      do_test
    fi ;;
  *)
    echo "Usage: $0 [build | run | test | all]" ;;
esac
